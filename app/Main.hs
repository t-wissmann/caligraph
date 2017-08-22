module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser

import Data.Either
import Control.Monad

main :: IO ()
main = do
  p <- parse path
  case p of
    Left e -> do putStrLn ("Error parsing " ++ path ++ ":")
                 print e
    Right r -> mapM_ print r
  where
    path = "/home/thorsten/.reminders.d/thorsten-urlaub.rem"
