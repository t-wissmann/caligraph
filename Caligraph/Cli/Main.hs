{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr, cropTopBy, cropBottomBy,setAvailableSize)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style

import qualified Caligraph.Cli.DayGrid as DayGrid


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

import Lens.Micro
import Lens.Micro.TH

type State = DayGrid.St



binds = Map.fromList
  [ (KEsc, halt)
  , (KChar 'q', halt)
  , (KChar 'o', gotoToday)

  -- hjkl
  , (KChar 'h', switchDay (-1))
  , (KChar 'j', switchDay  7)
  , (KChar 'k', switchDay (-7))
  , (KChar 'l', switchDay  1)
  -- arrow keys
  , (KLeft , switchDay (-1))
  , (KDown , switchDay  7)
  , (KUp   , switchDay (-7))
  , (KRight, switchDay  1)
  ]

switchDay delta s =
  continue (s & DayGrid.focusDay %~ addDays delta & DayGrid.scrollToFocus)

gotoToday s =
  continue (s & DayGrid.focusDay .~ (s^.DayGrid.today) & DayGrid.scrollToFocus)


day2widget :: Day -> Widget n
day2widget day = str (formatTime defaultTimeLocale "%d. %b %Y" day)






tryEnableMouse :: EventM n ()
tryEnableMouse = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ do
      setMode output Mouse True
  return ()


mainApp :: App State () ()
mainApp =
  App { appDraw = (\s -> [DayGrid.render day2widget s])
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = myHandleEvent
      , appStartEvent = (\s -> tryEnableMouse >> return s)
      , appAttrMap = const $ attrMap defAttr
        [ ("focusDay", bg black)
        , ("cellBorder", fg white)
        ]
      }

myHandleEvent s (VtyEvent e) =
  case e of
    EvKey KEsc mods ->
      halt s
    EvKey key mods ->
      case Map.lookup key binds of
        Just cb -> cb s
        Nothing -> continue s
    EvResize w h ->
      continue (s & DayGrid.size .~ (w,h)
                  & DayGrid.computeVisibleRows
                  & DayGrid.scrollToFocus)
    EvMouseDown _ _ BScrollDown _ ->
      continue (s & DayGrid.scrollOffset %~ ((+) 3)
                  & DayGrid.computeVisibleRows)
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & DayGrid.scrollOffset %~ (\x -> (x - 3))
                  & DayGrid.computeVisibleRows)
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
  customMain buildVty Nothing mainApp (DayGrid.init size today)
  return ()

