module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Data.Time.Calendar
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import qualified Data.Map.Strict as Map

ui :: Int -> [Widget ()]
ui s = [str (show s) <+> vBorder <+> str "World!"]

binds = Map.fromList
  [ (KEsc, halt)
  , (KChar 'q', halt)
  , (KChar 'j', (\s -> continue (s + 1)))
  , (KChar 'k', (\s -> continue (s - 1)))
  ]



mainApp :: App Int () ()
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
  defaultMain mainApp 0
  return ()

