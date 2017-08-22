module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser
import Caligraph.Cli.Main as CliM

import Data.Either
import Control.Monad
import System.Environment

mainOld :: IO ()
mainOld = do
  args <- getArgs
  p <- parse (args !! 0)
  forM_ p $ \x -> case x of
    Left e -> do putStr ("Error parsing ")
                 print e
    Right r -> print r

main = CliM.testmain
