module Caligraph.Cli.Main where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

testmain :: IO ()
testmain = simpleMain ui
