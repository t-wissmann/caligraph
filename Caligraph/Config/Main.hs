module Caligraph.Config.Main where

import Caligraph.Utils (mapLeft)

import Data.Ini
import Data.Text
import Data.Text.IO as T
import Data.HashMap.Strict as M
import System.Environment.XDG.BaseDir
import Control.Exception
import System.IO.Error

data Config = Config {
  environment :: M.HashMap Text Text
  }

type SectionParser a = M.HashMap Text Text -> Either String a

load :: IO (Either String Config)
load = do
    path <- getUserConfigFile "caligraph" "config.ini"
    src <- getSource path
    return $ do
        ini <- parseIni src
        let section = parseSection ini
        return Config
          <*> parseSection ini "environment" parseEnvironment
    where
      getSource :: String -> IO Text
      getSource path = handle readHandler $ T.readFile path
      readHandler :: IOError -> IO Text
      readHandler e
        | isDoesNotExistError e = return $ pack ""
        | otherwise = ioError e

parseSection :: Ini -> String -> SectionParser a -> Either String a
parseSection (Ini ini) sec_name parser =
    mapLeft (\s -> "In section \"" ++ sec_name ++ "\": " ++ s) $
    case M.lookup (pack sec_name) ini of
      Just values -> parser values
      Nothing -> parser M.empty


parseEnvironment :: SectionParser (M.HashMap Text Text)
parseEnvironment = Right

