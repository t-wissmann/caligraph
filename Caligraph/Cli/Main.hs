{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr,vBox,(<+>))
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style

import Caligraph.Cli.Types
import qualified Caligraph.Cli.DayGrid as DayGrid
import qualified Caligraph.Cli.DayWidget as DayWidget

import Caligraph.Utils
import qualified Caligraph.Config.Calendars as Config
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Calendar as CC

import Control.Monad (when)
import Control.Monad.State (runState, get)
import Control.Monad.IO.Class (liftIO)
import Data.Array
import Data.Maybe
import qualified Caligraph.Cli.UnicodeJunction as UJ

import qualified Data.Text as T
import Data.Time.Calendar
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty (outputIface)
import Graphics.Vty.Output.Interface (supportsMode,Mode(Mouse),setMode)
import qualified Data.Map.Strict as Map
import System.Exit
import System.Environment (getArgs)

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

data St = St
    { _dayGrid :: DayGrid.St WidgetName
    , _visibleIncarnations :: Array Day [CB.Incarnation]
    , _focusItem :: Maybe Int -- the item focused within a day, Nothing means 'the last'
    , _calendar :: CC.Calendar
    }

makeLenses ''St


binds :: Map.Map ([Modifier],Key) (St -> EventM WidgetName (Next St))
binds = Map.fromList
  [ (([], KEsc), halt)
  , (([], KChar 'q'), halt)
  , (([], KChar 'o'), c $ DayGrid.gotoToday)

  , (([MCtrl], KChar 'd'), c $ DayGrid.scrollPage 0.45)
  , (([MCtrl], KChar 'u'), c $ DayGrid.scrollPage (-0.45))
  , (([MCtrl], KChar 'f'), c $ DayGrid.scrollPage 0.90)
  , (([MCtrl], KChar 'b'), c $ DayGrid.scrollPage (-0.90))

  -- hjkl
  , (([], KChar 'h'), focus_cmd DirLeft)
  , (([], KChar 'j'), focus_cmd DirDown)
  , (([], KChar 'k'), focus_cmd DirUp)
  , (([], KChar 'l'), focus_cmd DirRight)
  -- arrow keys
  , (([], KLeft ), focus_cmd DirLeft)
  , (([], KDown ), focus_cmd DirDown)
  , (([], KUp   ), focus_cmd DirUp)
  , (([], KRight), focus_cmd DirRight)
  ]
  where c f = (\st -> continue (st & dayGrid %~ f))


focus_cmd :: Dir -> St -> EventM WidgetName (Next St)
focus_cmd dir st =
    continue (
        case reminderInDir of
            Right idx ->
                st & focusItem .~ Just idx
            Left idx ->
                st
                & focusItem .~ idx
                & dayGrid %~ DayGrid.moveFocus dir)
    where
      focus = st^.dayGrid^.DayGrid.focusDay
      reminders = (fromMaybe [] $ safeArray (st^.visibleIncarnations) focus)
      focusItemConcrete =
        case (st^.focusItem) of
            Nothing -> length reminders - 1
            Just idx -> idx
      reminderInDir :: Either (Maybe Int) Int
      reminderInDir =
        case dir of
            DirLeft -> Left (Just 0)
            DirRight -> Left (Just 0)
            DirUp ->
                if focusItemConcrete > 0
                then Right $ focusItemConcrete - 1
                else Left Nothing
            DirDown ->
                if focusItemConcrete + 1 < length reminders
                then Right $ focusItemConcrete + 1
                else Left (Just 0)

ui st =
  [DayGrid.render $ st^.dayGrid]



tryEnableMouse :: EventM WidgetName ()
tryEnableMouse = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ do
      setMode output Mouse True
  return ()


mainApp :: App St () WidgetName
mainApp =
  App { appDraw = ui
      , appChooseCursor = showFirstCursor
      , appHandleEvent = (\s ev ->
            do
            dg <- DayGrid.updateWidgetSize (s^.dayGrid)
            s' <- return (s & (dayGrid .~ dg))
            myHandleEvent s' ev
        )
      , appStartEvent = (\s -> tryEnableMouse >> return (updateDayRange s))
      , appAttrMap = const $ attrMap defAttr
        [ ("cellBorder", fg white)
        , ("cellHeader", yellow `on` black)
        , ("cellHeaderFocus", yellow `on` black)
        , ("cellHeaderToday", black `on` yellow)
        , ("cellHeaderFocusToday", black `on` yellow)
        , ("reminderTitle", defAttr)
        , ("reminderTime", Attr (SetTo bold) (SetTo green) KeepCurrent)
        , ("selectedReminderTitle", bg black)
        , ("selectedReminderTime", Attr (SetTo bold) (SetTo green) (SetTo black))
        ]
      }

scrollStep = 3

myHandleEvent :: St -> BrickEvent WidgetName () -> EventM WidgetName (Next St)
myHandleEvent s (VtyEvent e) =
  case e of
    EvKey KEsc mods ->
      halt s
    EvKey key mods ->
      case Map.lookup (mods,key) binds of
        Just cb -> fmap (fmap updateDayRange) (cb s)
        Nothing -> continue (updateDayRange s)
    EvResize w h ->
      continue (s & dayGrid %~ DayGrid.resize (w,h) & updateDayRange)
    EvMouseDown _ _ BScrollDown _ ->
      continue (s & dayGrid %~ DayGrid.scroll scrollStep & updateDayRange)
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & dayGrid %~ DayGrid.scroll (-scrollStep) & updateDayRange)
    _ ->
      continue s
myHandleEvent s (AppEvent ()) = continue s
myHandleEvent s (MouseDown (WNDay d) BLeft _ _) =
      continue (s & dayGrid %~ DayGrid.setFocus d & focusItem .~ Just 0 & updateDayRange)
myHandleEvent s (MouseDown (WNDayItem d idx) BLeft _ _) =
      continue (s & dayGrid %~ DayGrid.setFocus d & focusItem .~ Just idx & updateDayRange)
myHandleEvent s (MouseDown _ BScrollDown _ _) =
      continue (s & dayGrid %~ DayGrid.scroll scrollStep & updateDayRange)
myHandleEvent s (MouseDown _ BScrollUp _ _) =
      continue (s & dayGrid %~ DayGrid.scroll (-scrollStep) & updateDayRange)
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s

day2widget :: St -> Day -> DayWidget WidgetName
day2widget st day =
    DayWidget.day2widget
        (DayWidget.St
            (if focus == day
              then Just $ fromMaybe (length reminders - 1) (st^.focusItem)
              else Nothing)
            reminders
            day
            today)
    where
      today = st^.dayGrid^.DayGrid.today
      focus = st^.dayGrid^.DayGrid.focusDay
      reminders = (fromMaybe [] $ safeArray (st^.visibleIncarnations) day)

updateDayRange :: St -> St
updateDayRange st = st &~ do
    if day_range == bounds (st^.visibleIncarnations)
      then return ()
      else do
        incs <- zoom calendar $ CC.query day_range
        visibleIncarnations .= incs
    s <- get
    dayGrid %= (DayGrid.resizeDays $ day2widget s)
    where
    day_range = (DayGrid.rangeVisible $ st^.dayGrid)

testmain :: IO ()
testmain = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  today <- DayGrid.getToday
  args <- getArgs
  raw_calendars <- Config.load >>= rightOrDie
  cal <- rightOrDie $ CC.fromConfig (snd $ raw_calendars !! 0)
  cal_loaded <-
    case CC.dequeueIO cal of
        Nothing -> return cal
        Just io_action -> io_action
  customMain buildVty Nothing mainApp
    (St
        (DayGrid.init WNDayGrid today)
        (array (today,addDays (-1) today) [])
        (Just 0)
        cal_loaded)
  return ()

rightOrDie :: Either String a -> IO a
rightOrDie = either die return

