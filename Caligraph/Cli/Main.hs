module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Data.Time.Calendar
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Map.Strict as Map

ui :: State -> [Widget ()]
ui s = [str (show $ focusDay s) <+> vBorder <+> str "World!"]

binds = Map.fromList
  [ (KEsc, halt)
  , (KChar 'q', halt)
  , (KChar 'j', switchDay (-1))
  , (KChar 'k', switchDay 1)
  ]

switchDay delta s =
  continue (s { focusDay = addDays delta (focusDay s) })


data State = State
  { scrollOffset :: Int
  , scrollDay :: Day
  , currentDay :: Day
  , focusDay :: Day
  }

stateToday :: IO State
stateToday = do
  g <- getCurrentTime
  let d = utctDay g
  return $ State 0 d d d

mainApp :: App State () ()
mainApp =
  App { appDraw = ui
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = myHandleEvent
      , appStartEvent = return
      , appAttrMap = const $ attrMap defAttr []
      }

myHandleEvent s (VtyEvent e) =
  case e of
    EvKey KEsc mods ->
      halt s
    EvKey key mods ->
      case Map.lookup key binds of
        Just cb -> cb s
        Nothing -> continue s
    _ ->
      continue s

myHandleEvent s (AppEvent ()) = continue s
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s


testmain :: IO ()
testmain = do
  s <- stateToday
  defaultMain mainApp s
  return ()

