
module Config where

import Caligraph.Config.Main
import Caligraph.Config.Types
import Tester
import Text.Read
import Control.Monad

exampleKeyCombis =
  [ "Shift-Home"
  , "Ctrl-Up"
  , "Meta-minus"
  , "minus"
  , "Home"
  , "d"
  , "+"
  , "Ctrl-Shift->"
  , "Ctrl-Shift-/"
  ]

testConfig :: TestM ()
testConfig = do
  forM_ exampleKeyCombis (\kombi -> do
    k <- userRead kombi
    kombi =!= userShow (k :: KeyCombi))
  forM_ ["ome", "Shif-x", "Ctrl-esc", "Ctrl+c"] (\wrongCombi -> do
    isLeft (userRead wrongCombi :: Either String KeyCombi)
    )
