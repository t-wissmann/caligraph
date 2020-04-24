module Main where

import Caligraph.Lib
import Caligraph.Remind.Parser
import qualified Caligraph.Cli.Main as CliM
import qualified Caligraph.Calendar as CC
import qualified Caligraph.Backend.Registered as CB

import qualified Caligraph.Config.Main as Cfg
import qualified Caligraph.Config.Defaults as Cfg
import qualified Caligraph.Config.Calendars as Cfg
import qualified Caligraph.Rules as Rules

import Data.Either
import Control.Monad
import System.Environment
import System.Exit
import Control.Monad.Trans.Except
import System.FilePath.Posix (takeExtension, takeBaseName)

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import Options.Applicative
import Data.Semigroup ((<>))
import Text.Regex
import Data.Maybe (isJust)

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
  cals <- case calendarfFile params of
          "" -> runExceptT (Cfg.loadCalendars CC.fromConfig) >>= rightOrDie
          path -> do
            c <- runExceptT (filepath2calendar path) >>= rightOrDie
            return [c]
  -- load rules
  rules <- runExceptT Rules.loadRules >>= rightOrDie
  -- start main application
  CliM.mainFromConfig rules (Map.union customBinds defaultBinds) cals
  where
    rightOrDie = either die return

filepath2calendar :: FilePath -> ExceptT String IO (T.Text,CC.ConfiguredCalendar)
filepath2calendar filepath = do
  let possibleBackends = filter (\(r,_) -> r `matches` filepath) CB.pathRegex2backend
  backName <- case possibleBackends of
              [] -> throwE $ "Can not detect file type of " ++ filepath
              ((_,back):_) -> return back
  calCfg <- except $ Cfg.parseCalendar $ HMap.fromList $ toText
            [ ("type", backName)
            , ("path", filepath)
            , ("color", "blue")
            , ("color-inv", "default")
            ]
  (,) (T.pack $ takeBaseName filepath) <$> CC.fromConfig calCfg
  where
    matches reg str = isJust $ matchRegex (mkRegexWithOpts reg False False) str
    toText = map (\(a,b) -> (T.pack a, T.pack b))
