module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser
import qualified Caligraph.Cli.Main as CliM
import qualified Caligraph.Calendar as CC

import qualified Caligraph.Config.Main as Cfg
import qualified Caligraph.Config.Defaults as Cfg
import qualified Caligraph.Config.Calendars as Cfg

import Data.Either
import Control.Monad
import System.Environment
import System.Exit
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as Map
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
  res <- execParser opts
  mainConfigurable res
  where
    opts = info (optparse <**> helper)
      ( fullDesc
      <> progDesc "Caligraph - calendar interactive and graphical")

mainConfigurable :: CliOpts -> IO ()
mainConfigurable params = do
  -- load main config
  config <- Cfg.load >>= rightOrDie
  Cfg.evaluateEnvironmentConfig (Cfg.environment config)
  -- load key config
  customBinds <- Cfg.keyConfigUserPath >>= Cfg.getSource >>= CliM.loadKeyConfig
  defaultBinds <- CliM.loadKeyConfig Cfg.defaultKeys
  -- load calendar config
  cals <- runExceptT (Cfg.loadCalendars CC.fromConfig) >>= rightOrDie
  CliM.mainFromConfig (Map.union customBinds defaultBinds) cals
  where
    rightOrDie = either die return

