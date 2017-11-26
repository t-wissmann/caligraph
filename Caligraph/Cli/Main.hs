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

import Control.Monad (when)
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
import qualified Caligraph.Backend as CB
import qualified Caligraph.Remind.Backend as Remind
import System.Exit
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

import Lens.Micro
import Lens.Micro.TH

data St = St
    { _dayGrid :: DayGrid.St WidgetName
    , _backend :: CB.Backend
    , _visibleIncarnations :: Array Day [CB.Incarnation]
    , _focusItem :: Int -- the item focused within a day
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
    continue (st & dayGrid %~ DayGrid.moveFocus dir)
    where
      focus = st^.dayGrid^.DayGrid.focusDay
      reminders = (fromMaybe [] $ safeArray (st^.visibleIncarnations) focus)

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
        , ("reminderTime", Attr (SetTo bold) (SetTo green) KeepCurrent)
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
      continue (s & dayGrid %~ DayGrid.setFocus d & updateDayRange)
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
              then Just (st^.focusItem)
              else Nothing)
            reminders
            day
            today)
    where
      today = st^.dayGrid^.DayGrid.today
      focus = st^.dayGrid^.DayGrid.focusDay
      reminders = (fromMaybe [] $ safeArray (st^.visibleIncarnations) day)

updateDayRange :: St -> St
updateDayRange st =
    st
    & (if day_range == bounds (st^.visibleIncarnations)
      then id
      else visibleIncarnations .~ CB.query (st^.backend) day_range)
    & (\s -> s & dayGrid %~ (DayGrid.resizeDays $ day2widget s))
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
  calendars <- Config.load >>= rightOrDie
  p <- (expandTilde $ Config.path $ snd $ calendars !! 0)
  backend <- Remind.init p
  customMain buildVty Nothing mainApp
    (St
        (DayGrid.init WNDayGrid today)
        backend
        (array (today,addDays (-1) today) [])
        0)
  return ()

rightOrDie :: Either String a -> IO a
rightOrDie = either die return

expandTilde :: FilePath -> IO FilePath
expandTilde s =
    case s of
     ['~'] -> getHomeDirectory
     ('~':'/':tl) -> do
        home <- getHomeDirectory
        return $ joinPath [home, tl]
     x -> return x

