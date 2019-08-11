module Caligraph.Config.Main where

import Caligraph.Utils (mapLeft)
import Caligraph.Config.Types

import Data.Ini
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M
import System.Environment.XDG.BaseDir
import Control.Exception
import Control.Monad
import Text.Read
import qualified Data.List.Split as Split
import System.IO.Error

data Config = Config {
  environment :: M.HashMap Text Text
  }

type SectionParser a = M.HashMap Text Text -> Either String a

getSource :: String -> IO Text
getSource path = handle readHandler $ T.readFile path
readHandler :: IOError -> IO Text
readHandler e
  | isDoesNotExistError e = return $ pack ""
  | otherwise = ioError e

load :: IO (Either String Config)
load = do
    path <- getUserConfigFile "caligraph" "config.ini"
    src <- getSource path
    return $ do
        ini <- parseIni src
        let section = parseSection ini
        return Config
          <*> parseSection ini "environment" parseEnvironment

parseSection :: Ini -> String -> SectionParser a -> Either String a
parseSection ini sec_name parser =
    mapLeft (\s -> "In section \"" ++ sec_name ++ "\": " ++ s) $
    case M.lookup (pack sec_name) (unIni ini) of
      Just values -> parser values
      Nothing -> parser M.empty

parseEnvironment :: SectionParser (M.HashMap Text Text)
parseEnvironment = Right

data KeyConfig = KeyConfig { globalKeys :: [(KeyCombi, [String])] } -- | a mapping of strings to commands

-- | filepath to user's key config
keyConfigUserPath :: IO FilePath
keyConfigUserPath = getUserConfigFile "caligraph" "keys.ini"

parseKeyConfig :: Text -> Either String KeyConfig
parseKeyConfig src = fmap KeyConfig $ do
  ini <- parseIni src
  let sec = maybe [] id $ fmap M.toList $ M.lookup (pack "") (unIni ini)
  forM sec (\(key,value) -> do
      keyParsed <- mapLeft (\_ -> "Invalid key combi " ++ unpack key)
                   (readEither $ unpack key)
      let command = Split.splitOn " " (unpack value)
      return (keyParsed :: KeyCombi, command :: [String]))
