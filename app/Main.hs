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
import qualified Caligraph.Headless as Headless

import Data.Either
import Control.Monad
import System.Environment
import System.Exit
import Control.Monad.Trans.Except
import System.FilePath.Posix (takeExtension, takeBaseName)
import System.Environment.XDG.BaseDir

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import Options.Applicative
import Data.Semigroup ((<>))
import Text.Regex
import Data.Maybe (isJust)

data CliOpts = CliOpts
  { calendarfFile :: FilePath
  -- ^ filepath of a particular calendar to open
  , calendarsIni :: FilePath
  -- ^ filepath of the calendars.ini file
  , coCommand :: MainCommand
  }

data MainCommand = McUi | McPrint Headless.HeadlessOptions


mainOld :: IO ()
mainOld = do
  args <- getArgs
  p <- parseFile (args !! 0)
  forM_ p $ \x -> case x of
    Left e -> do putStr ("Error parsing ")
                 print e
    Right r -> print r

optparseInfo :: ParserInfo CliOpts
optparseInfo = info (optparse <**> helper)
      ( fullDesc
      <> progDesc "Caligraph - calendar interactive and graphical")

optparse :: Parser CliOpts
optparse = CliOpts
  <$> strOption
    ( long "file"
    <> metavar "FILE"
    <> short 'f'
    <> value ""
    <> help "Calendar file to open (instead of configured calendars)" )
  <*> strOption
    ( long "calendars"
    <> metavar "CALENDARSINI"
    <> value ""
    <> help "Path to the calendars.ini configuration file"
    <> showDefaultWith (const "~/.config/caligraph/calendars.ini")
    )
  <*> (hsubparser
    ( command "ui" (info (pure McUi) (progDesc "interactive mode"))
    <> command "print" (info (McPrint <$> printOptions) (progDesc "print calendar to stdout"))
    )
    <|> pure McUi)

printOptions :: Parser Headless.HeadlessOptions
printOptions = Headless.HeadlessOptions
  <$> option (Just <$> auto)
    ( long "from"
    <> metavar "DAY"
    <> value Nothing
    <> help "First to day to include in the output (YYYY-MM-DD)"
    )
  <*> option (Just <$> auto)
    ( long "to"
    <> metavar "DAY"
    <> value Nothing
    <> help "Last to day to include in the output (YYYY-MM-DD)"
    )
  <*> option (maybeReader $ \s -> Just $ Just $ mkRegexWithOpts s False False)
    ( long "title"
    <> metavar "REGEX"
    <> value Nothing
    <> help "Regex to filter entries by their title"
    )
  <*> option (eitherReader Headless.parseOutputFormat)
    ( long "format"
    <> metavar "OUTPUTFORMAT"
    <> value Headless.OfJson
    <> help "Format of the output"
    )

getCalendarsIniPath :: IO FilePath
getCalendarsIniPath = getUserConfigFile "caligraph" "calendars.ini"

main :: IO ()
main = do
  res <- execParser optparseInfo
  case coCommand res of
    McUi -> mainConfigurable res
    McPrint opts -> mainScript res opts

mainConfigurable :: CliOpts -> IO ()
mainConfigurable params = do
  -- load main config
  config <- Cfg.load >>= rightOrDie
  Cfg.evaluateEnvironmentConfig (Cfg.environment config)
  -- load key config
  customBinds <- do
    src <- Cfg.keyConfigUserPath >>= Cfg.getSource
    ini <- runExceptT (Cfg.parseConfigFile src) >>= rightOrDie
    CliM.loadKeyConfig ini
  defaultBinds <- do
    ini <- runExceptT (Cfg.parseConfigFile Cfg.defaultKeys) >>= rightOrDie
    CliM.loadKeyConfig ini
  -- load calendar config
  cals <- loadCalendars params
  -- load rules
  rules <- runExceptT Rules.loadRules >>= rightOrDie
  -- start main application
  CliM.mainFromConfig rules (Map.union customBinds defaultBinds) cals

rightOrDie = either die return

mainScript :: CliOpts -> Headless.HeadlessOptions -> IO ()
mainScript globalParams scriptParams = do
  -- load main config
  config <- Cfg.load >>= rightOrDie
  Cfg.evaluateEnvironmentConfig (Cfg.environment config)
  -- load calendar config
  cals <- loadCalendars globalParams
  Headless.main cals scriptParams

loadCalendars :: CliOpts -> IO [(T.Text,CC.ConfiguredCalendar)]
loadCalendars params =
  case calendarfFile params of
    "" -> do
      calendars_ini <- if calendarsIni params /= ""
                       then return (calendarsIni params)
                       else getCalendarsIniPath
      runExceptT (Cfg.loadCalendars CC.fromConfig calendars_ini)
          >>= rightOrDie
    path -> do
      c <- runExceptT (filepath2calendar path) >>= rightOrDie
      return [c]

filepath2calendar :: FilePath -> ExceptT String IO (T.Text,CC.ConfiguredCalendar)
filepath2calendar filepath = do
  let possibleBackends = filter (\(r,_) -> r `matches` filepath) CB.pathRegex2backend
  backName <- case possibleBackends of
              [] -> throwE $ "Can not detect file type of " ++ filepath
              ((_,back):_) -> return back
  calCfg <- except $ Cfg.parseCalendar "" $ HMap.fromList $ toText
            [ ("type", backName)
            , ("path", filepath)
            , ("color", "blue")
            , ("color-inv", "default")
            ]
  (,) (T.pack $ takeBaseName filepath) <$> CC.fromConfig calCfg
  where
    matches reg str = isJust $ matchRegex (mkRegexWithOpts reg False False) str
    toText = map (\(a,b) -> (T.pack a, T.pack b))
