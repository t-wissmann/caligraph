module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser

import Control.Monad

main :: IO ()
main =
  forM_ parse $ \x ->
    putStrLn $ show x
