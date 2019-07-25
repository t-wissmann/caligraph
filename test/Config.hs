
module Config where

import Caligraph.Config.Main
import Tester
import Text.Read
import Control.Monad

exampleKeyCombis =
  [ "Shift-Home"
  , "Ctrl-Up"
  , "Meta-minus"
  , "minus"
  , "d"
  , "+"
  , "Ctrl-Shift->"
  , "Ctrl-Shift-/"
  ]

testConfig :: TestM ()
testConfig = do
  forM_ exampleKeyCombis (\kombi ->
    kombi =!= show ((read kombi) :: KeyCombi))
  forM_ ["ome", "Shif-x", "Ctrl-esc", "Ctrl+c"] (\wrongCombi ->
    Nothing =!= (readMaybe wrongCombi :: Maybe KeyCombi))
