module Caligraph.Config.Calendars where

import Caligraph.Config.Main
import Caligraph.Config.Types
import Data.HashMap.Strict as M
import Caligraph.Utils (mapLeft)

import Data.Text
import Data.Text.IO as T
import Data.Ini
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad

import Graphics.Vty.Attributes

data CalendarConfig = CalendarConfig
    { backendType :: String
    , configFilePath :: FilePath
    -- ^ the filepath of this config file
    , color :: Maybe Color
    -- ^ the main color of this calendar (Nothing = default)
    , colorInv :: Maybe Color
    -- ^ a contrast color (Nothing = default)
    , allSettings :: HashMap Text Text
    }

type RawCalList = [(Text, CalendarConfig)]

-- | load the calendars.ini file
loadConfig
    :: FilePath
    -- ^ the path to the calendars.ini file
    -> ExceptT String IO RawCalList
    -- ^ the loaded calendar list
loadConfig path = do
    src <- liftIO $ T.readFile path
    ini <- parseConfigFile src
    mapM (parseSection path) $ M.toList $ unIni ini
    where
    parseSection path (a,b) =
        withExceptT (\s -> "In section \"" ++ unpack a ++ "\": " ++ s) $
        return ((,) a) <*> except (parseCalendar path b)

loadCalendars
    :: (CalendarConfig -> ExceptT String IO a)
    -- ^ initialization of calendars, hidden as the type 'a'
    -> FilePath
    -- ^ the filepath to the calendars.ini file
    -> ExceptT String IO [(Text, a)]
loadCalendars fromConfig calendars_ini = do
  raw_calendars <- loadConfig calendars_ini
  forM raw_calendars (\(t,c) -> do
    c' <- fromConfig c
    return (t,c'))

parseCalendar
    :: FilePath
    -- ^ the filepath of the config file
    -> SectionParser CalendarConfig
    -- ^ a section parser
parseCalendar configFilePath section =
    return CalendarConfig
      <*> f "type"
      <*> pure configFilePath
      <*> withDefault "color" Nothing uiColor
      <*> withDefault "color-inv" Nothing uiColor
      <*> pure section
    where
      f = field section
      withDefault key defaultValue f =
        mapLeft (\s -> "In key \"" ++ key ++ "\": " ++ s) $
        case M.lookup (pack key) section of
            Nothing -> return defaultValue
            Just v ->
              mapLeft (\s -> "invalid value \"" ++ unpack v ++ "\": " ++ s) $
              fmap f $ userRead $ unpack v


field :: HashMap Text Text -> String -> Either String String
field section key =
    case M.lookup (pack key) section of
        Nothing ->
            Left ("key \"" ++ key ++ "\" required but missing")
        Just v ->
            Right (unpack v)

