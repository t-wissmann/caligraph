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


import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

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

import Lens.Micro
import Lens.Micro.TH

data St = St
    { _dayGrid :: DayGrid.St
    , _backend :: CB.Backend
    }

makeLenses ''St


binds :: Map.Map Key (St -> EventM () (Next St))
binds = Map.fromList
  [ (KEsc, halt)
  , (KChar 'q', halt)
  , (KChar 'o', c $ DayGrid.gotoToday)

  -- hjkl
  , (KChar 'h', c $ DayGrid.moveFocus DirLeft)
  , (KChar 'j', c $ DayGrid.moveFocus DirDown)
  , (KChar 'k', c $ DayGrid.moveFocus DirUp)
  , (KChar 'l', c $ DayGrid.moveFocus DirRight)
  -- arrow keys
  , (KLeft , c $ DayGrid.moveFocus DirLeft)
  , (KDown , c $ DayGrid.moveFocus DirDown)
  , (KUp   , c $ DayGrid.moveFocus DirUp)
  , (KRight, c $ DayGrid.moveFocus DirRight)
  ]
  where c f = (\st -> continue (st & dayGrid %~ f))


day2widget :: St -> Day -> Widget n
day2widget st day =
    (withAttr headerAttr $
        hCenter $
            str $ formatTime defaultTimeLocale day_format day)
    <=> reminders
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
      reminders =
        CB.query (st^.backend) day
        & concatMap (\x -> CB.incarnations x day day)
        & map reminderWidget
        & vBox
      reminderWidget r =
        str " " <=>
        str (CB.title r)






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
  App { appDraw = (\s -> [DayGrid.render (day2widget s) $ s^.dayGrid])
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = myHandleEvent
      , appStartEvent = (\s -> tryEnableMouse >> return s)
      , appAttrMap = const $ attrMap defAttr
        [ ("cellBorder", fg white)
        , ("cellHeader", fg white)
        , ("cellHeaderFocus", defAttr)
        , ("cellHeaderToday", white `on` black)
        , ("cellHeaderFocusToday", bg black)
        ]
      }

myHandleEvent :: St -> BrickEvent () () -> EventM () (Next St)
myHandleEvent s (VtyEvent e) =
  case e of
    EvKey KEsc mods ->
      halt s
    EvKey key mods ->
      case Map.lookup key binds of
        Just cb -> cb s
        Nothing -> continue s
    EvResize w h ->
      continue (s & dayGrid %~ DayGrid.resize (w,h))
    EvMouseDown _ _ BScrollDown _ ->
      continue (s & dayGrid %~ DayGrid.scroll 3)
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & dayGrid %~ DayGrid.scroll (-3))
    _ ->
      continue s

myHandleEvent s (AppEvent ()) = continue s
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s


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
  customMain buildVty Nothing mainApp (St (DayGrid.init size today) backend)
  return ()

