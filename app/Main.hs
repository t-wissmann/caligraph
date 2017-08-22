module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser

import Data.Either
import Control.Monad
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  p <- parse (args !! 0)
  forM_ p $ \x -> case x of
    Left e -> do putStr ("Error parsing ")
                 print e
    Right r -> print r
