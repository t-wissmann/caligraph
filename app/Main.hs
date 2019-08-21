module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser
import Caligraph.Cli.Main as CliM

import Data.Either
import Control.Monad
import System.Environment

import Options.Applicative
import Data.Semigroup ((<>))

data CliOpts = CliOpts
  { calendarfFile :: FilePath
  }

mainOld :: IO ()
mainOld = do
  args <- getArgs
  p <- parseFile (args !! 0)
  forM_ p $ \x -> case x of
    Left e -> do putStr ("Error parsing ")
                 print e
    Right r -> print r

optparse :: Parser CliOpts
optparse = CliOpts
  <$> strOption
    ( long "file"
    <> metavar "FILE"
    <> short 'f'
    <> value ""
    <> help "Calendar file to open (instead of configured calendars)" )

main :: IO ()
main = do
  -- res <- execParser opts
  CliM.testmain
  where
    opts = info (optparse <**> helper)
      ( fullDesc
      <> progDesc "Caligraph - calendar interactive and graphical")
