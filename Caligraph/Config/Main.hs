module Caligraph.Config.Main where

import Caligraph.Utils (mapLeft)
import Caligraph.Config.Types

import Data.Ini
import Data.Text (Text, pack, unpack, isPrefixOf, isSuffixOf)
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M
import System.Environment.XDG.BaseDir
import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Text.Read
import System.Process (shell, readProcessWithExitCode)
import qualified Data.List.Split as Split
import System.IO.Error
import System.Exit
import System.IO (hPutStrLn, hPutStr, stderr)
import System.Environment (getArgs,setEnv)

data Config = Config {
  environment :: M.HashMap Text Text
  }


evaluateEnvironmentConfig :: M.HashMap Text Text -> IO ()
evaluateEnvironmentConfig envCfg =
  forM_ (M.toList envCfg) (\(k,v) ->
    setEnv (unpack k) (unpack v))

getSource :: String -> IO Text
getSource path = handle readHandler $ T.readFile path

readHandler :: IOError -> IO Text
readHandler e
  | isDoesNotExistError e = return $ pack ""
  | otherwise = ioError e

-- | load a config file with given name. If no such file exists,
-- an empty config is returned
loadConfigFile
  :: String
  -- ^ the name, e.g. config, rules, keys,…
  -> ExceptT String IO Ini
  -- ^ return an error message or the file
loadConfigFile fileName = do
    path <- liftIO $ getUserConfigFile "caligraph" (fileName ++ ".ini")
    src <- liftIO $ getSource path
    parseConfigFile src


-- | traverse and edit the values (i.e. right-hand sides) in an ini file
traverseIniValues :: Applicative m => (Text -> m Text) -> Ini -> m Ini
traverseIniValues actionOnValues ini =
    Ini
    <$> (traverse (traverse (traverse actionOnValues)) (iniSections ini))
    <*> (traverse (traverse actionOnValues) (iniGlobals ini))


-- | read a ini file and do other custom post-processing.
-- This function should be used instead of `parseIni`
parseConfigFile :: Text -> ExceptT String IO Ini
parseConfigFile src = do
    plain_ini <- except $ parseIni src
    flip traverseIniValues plain_ini $ \value -> do
        if (pack "`") `isPrefixOf` value
        then
            if (pack "`") `isSuffixOf` value
            then liftIO $ do
                let shell_cmd = tail (dropLast (unpack value))
                -- hPutStrLn stderr ("RUNNING: >" ++ unpack value ++ "<")
                (code,out,err) <- readProcessWithExitCode "sh" ["-c", shell_cmd] ""
                hPutStr stderr err
                case code of
                    ExitSuccess -> return ()
                    ExitFailure c ->
                        hPutStrLn stderr ("WARNING: `" ++ shell_cmd ++ "` exited with code " ++ show c ++ ".")
                return (pack out)
            else do
                -- only print a warning
                liftIO $ hPutStrLn stderr ("WARNING: unmatched backtick in value \"" ++ unpack value ++ "\"")
                return value
        else
            return value
    where
        dropLast :: String -> String
        dropLast [] = []
        dropLast [x] = []
        dropLast (x:xs) = x : dropLast xs

load :: IO (Either String Config)
load = do
    ini' <- runExceptT $ loadConfigFile "config"
    return $ do
        ini <- ini'
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

-- | a mapping of strings to commands
data KeyConfig = KeyConfig { globalKeys :: [(KeyCombi, [String])] }

-- | filepath to user's key config
keyConfigUserPath :: IO FilePath
keyConfigUserPath = getUserConfigFile "caligraph" "keys.ini"

parseKeyConfig :: Ini -> Either String KeyConfig
parseKeyConfig ini = fmap KeyConfig $ do
  let sec = maybe [] id $ fmap M.toList $ M.lookup (pack "") (unIni ini)
  forM sec (\(key,value) -> do
      keyParsed <- mapLeft (\_ -> "Invalid key combi " ++ unpack key)
                   (userRead $ unpack key)
      let command = Split.splitOn " " (unpack value)
      return (keyParsed :: KeyCombi, command :: [String]))
