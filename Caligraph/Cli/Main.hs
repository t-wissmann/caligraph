{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr, cropTopBy, cropBottomBy,setAvailableSize)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (hCenter)

import qualified Caligraph.Cli.DayGrid as DayGrid
import Caligraph.Cli.DayGrid (Dir(DirUp,DirDown,DirLeft,DirRight))

import Caligraph.Utils

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Text.Wrap
import Data.Array
import Data.Maybe
import qualified Caligraph.Cli.UnicodeJunction as UJ

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty (outputIface)
import Graphics.Vty.Output.Interface (supportsMode,Mode(Mouse),setMode)
import qualified Data.Map.Strict as Map
import qualified Caligraph.Backend as CB
import qualified Caligraph.Remind.Backend as Remind
import System.Environment (getArgs)
import Data.List (sort, intersperse)

import Lens.Micro
import Lens.Micro.TH

data St = St
    { _dayGrid :: DayGrid.St ()
    , _backend :: CB.Backend
    , _visibleIncarnations :: Array Day [CB.Incarnation]
    }

makeLenses ''St


binds :: Map.Map ([Modifier],Key) (St -> EventM () (Next St))
binds = Map.fromList
  [ (([], KEsc), halt)
  , (([], KChar 'q'), halt)
  , (([], KChar 'o'), c $ DayGrid.gotoToday)

  , (([MCtrl], KChar 'd'), c $ DayGrid.scrollPage 0.45)
  , (([MCtrl], KChar 'u'), c $ DayGrid.scrollPage (-0.45))
  , (([MCtrl], KChar 'f'), c $ DayGrid.scrollPage 0.90)
  , (([MCtrl], KChar 'b'), c $ DayGrid.scrollPage (-0.90))

  -- hjkl
  , (([], KChar 'h'), c $ DayGrid.moveFocus DirLeft)
  , (([], KChar 'j'), c $ DayGrid.moveFocus DirDown)
  , (([], KChar 'k'), c $ DayGrid.moveFocus DirUp)
  , (([], KChar 'l'), c $ DayGrid.moveFocus DirRight)
  -- arrow keys
  , (([], KLeft ), c $ DayGrid.moveFocus DirLeft)
  , (([], KDown ), c $ DayGrid.moveFocus DirDown)
  , (([], KUp   ), c $ DayGrid.moveFocus DirUp)
  , (([], KRight), c $ DayGrid.moveFocus DirRight)
  ]
  where c f = (\st -> continue (st & dayGrid %~ f))

reminder2widget :: Int -> CB.Incarnation -> Int -> (Int, Widget n)
reminder2widget idx r width =
    ( length lines
    , updateAttrMap (applyAttrMappings [("reminder", mainAttribute)])
      $ withAttr "reminder"
      $ Widget Greedy Fixed $ do
        ctx <- getContext
        return $ emptyResult & imageL .~ (reminderImage (ctx^.attrL))
    )
  where
      mainAttribute =
        brightWhite `on` (if idx `mod` 2 == 0 then rgbColor 161 0 168 else rgbColor 99 0 103)
      titleWidth = (width - durationWidth)
      lines =
        (\l -> l ++ replicate (length duration - length l) "")
        $ wrapTextToLines
            (WrapSettings False True)
            titleWidth
            (T.pack $ CB.title r)
      durationWidth =
        if length duration == 0
        then 0
        else (+) 1 $ maximum $ map V.safeWcswidth duration
      placeholder_string =
        case duration of
            (_:_:_) -> "  |  "
            _       -> "     "
      placeholder =
        replicate (max 0 $ (length lines) - (length duration)) placeholder_string
      reminderImage attr =
        let
          safeTextWidth = V.safeWcswidth . T.unpack
          durationImg =
            V.vertCat
            $ map (\s -> V.string attr (s ++ " "))
            $ case duration of
                (hd:tl) -> hd : (placeholder ++ tl)
                [] -> []

          lineImg lStr = V.string attr (lStr ++ replicate (titleWidth - V.safeWcswidth lStr) ' ')
          lineImgs = lineImg <$> (map T.unpack lines)
        in
        V.horizJoin durationImg $ V.vertCat $ lineImgs

      duration =
        case (CB.time r, CB.duration r) of
            (Just (h,m), Just (dh,dm)) ->
                let
                  m' = m + dm
                  h' = h + dh + m' `div` 60
                  -- c1 = UJ.get UJ.Empty  UJ.Strong UJ.Normal UJ.Strong
                  -- c2 = UJ.get UJ.Normal UJ.Strong  UJ.Empty UJ.Strong
                  c1 = ':'
                  c2 = ':'
                in
                [ CB.showTime c1 (h,m)
                , CB.showTime c2 (h' `mod` 24, m' `mod` 60)
                ]
            (Just (h,m), Nothing) ->
                [CB.showTime ':' (h,m) ]
            (_, _) -> []

-- | return a widget for a day and its total height
day2widget :: St -> Day -> DayGrid.DayWidget n
day2widget st day width =
    (fromMaybe [] $ safeArray (st^.visibleIncarnations) day)
    & zipWith (\i d -> reminder2widget i d width) [0..]
    & intersperse (1, str " ") -- put empty lines in between
    & (:) (1, str " ") -- put an empty line below header
    & (:) headerWidget -- prepend header
    & unzip
    & (\(a,b) -> (sum a, vBox b))
    where
      today = st^.dayGrid^.DayGrid.today
      focus = st^.dayGrid^.DayGrid.focusDay
      headerAttr
        | day == focus && day == today = "cellHeaderFocusToday"
        | day == focus  = "cellHeaderFocus"
        | day == today  = "cellHeaderToday"
        | otherwise     = "cellHeader"
      (y,_,_) = toGregorian day
      (y_now,_,_) = toGregorian today
      day_format =
        if y == y_now then "%d. %b" else "%d. %b %Y"

      headerWidget =
        (,) 1
        $ withAttr headerAttr
            $ hCenter
            $ str
            $ formatTime defaultTimeLocale day_format day

ui st =
  [DayGrid.render $ st^.dayGrid]



tryEnableMouse :: EventM n ()
tryEnableMouse = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ do
      setMode output Mouse True
  return ()


mainApp :: App St () ()
mainApp =
  App { appDraw = ui
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = myHandleEvent
      , appStartEvent = (\s -> tryEnableMouse >> return (updateDayRange s))
      , appAttrMap = const $ attrMap defAttr
        [ ("cellBorder", fg white)
        , ("cellHeader", yellow `on` black)
        , ("cellHeaderFocus", yellow `on` black)
        , ("cellHeaderToday", black `on` yellow)
        , ("cellHeaderFocusToday", black `on` yellow)
        ]
      }

myHandleEvent :: St -> BrickEvent () () -> EventM () (Next St)
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
      continue (s & dayGrid %~ DayGrid.scroll 3 & updateDayRange)
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & dayGrid %~ DayGrid.scroll (-3) & updateDayRange)
    _ ->
      continue s
myHandleEvent s (AppEvent ()) = continue s
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s

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
  size <- DayGrid.getScreenSize
  today <- DayGrid.getToday
  args <- getArgs
  backend <- Remind.init (args !! 0)
  customMain buildVty Nothing mainApp
    (St
        (DayGrid.init size today)
        backend
        (array (today,addDays (-1) today) []))
  return ()

