module Caligraph.Config.Calendars where

import Caligraph.Config.Main
import Caligraph.Config.Types
import Data.HashMap.Strict as M
import Caligraph.Utils (mapLeft)

import Data.Text
import Data.Text.IO as T
import Data.Ini
import System.Environment.XDG.BaseDir
import Text.Read (readEither)

import Graphics.Vty.Attributes

data CalendarConfig = CalendarConfig
    { backendType :: String
    , color :: Maybe Color
    -- ^ the main color of this calendar (Nothing = default)
    , colorInv :: Maybe Color
    -- ^ a contrast color (Nothing = default)
    , allSettings :: HashMap Text Text
    }

type RawCalList = [(Text, CalendarConfig)]

load :: IO (Either String RawCalList)
load = do
    path <- getUserConfigFile "caligraph" "calendars.ini"
    src <- T.readFile path
    return $ do
        ini <- parseIni src
        mapM parseSection $ M.toList $ unIni ini
    where
    parseSection (a,b) =
        mapLeft (\s -> "In section \"" ++ unpack a ++ "\": " ++ s) $
        return ((,) a) <*> parseCalendar b


parseCalendar :: SectionParser CalendarConfig
parseCalendar section =
    return CalendarConfig
      <*> f "type"
      <*> withDefault "color" Nothing prettyColor
      <*> withDefault "color-inv" Nothing prettyColor
      <*> pure section
    where
      f = field section
      withDefault key defaultValue f =
        mapLeft (\s -> "In key \"" ++ key ++ "\": " ++ s) $
        case M.lookup (pack key) section of
            Nothing -> return defaultValue
            Just v ->
              mapLeft (\s -> "invalid value \"" ++ unpack v ++ "\": " ++ s) $
              fmap f $ readEither $ unpack v


field :: HashMap Text Text -> String -> Either String String
field section key =
    case M.lookup (pack key) section of
        Nothing ->
            Left ("key \"" ++ key ++ "\" required but missing")
        Just v ->
            Right (unpack v)

