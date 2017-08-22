module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser

import Control.Monad

main :: IO ()
main = do
  p <- parse "/home/thorsten/.reminders.d/termine.rem"
  forM_ p $ \x ->
    putStrLn $ show x
