module Caligraph.Config.Calendars where

import Data.HashMap.Strict as M

import Data.Text
import Data.Text.IO as T
import Data.Ini
import System.Environment.XDG.BaseDir

data CalendarConfig = CalendarConfig
    { backendType :: String
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


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left b) = Left $ f b
mapLeft _ (Right b) = Right b

parseCalendar :: HashMap Text Text -> Either String CalendarConfig
parseCalendar section =
    return CalendarConfig <*> f "type" <*> pure section
    where f = field section

field :: HashMap Text Text -> String -> Either String String
field section key =
    case M.lookup (pack key) section of
        Nothing ->
            Left ("key \"" ++ key ++ "\" required but missing")
        Just v ->
            Right (unpack v)

