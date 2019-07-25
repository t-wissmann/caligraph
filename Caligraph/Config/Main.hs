module Caligraph.Config.Main where

import Caligraph.Utils (mapLeft)

import Paths_Caligraph

import Data.Ini
import Data.Text (Text, pack, unpack)
import Data.List.Split (splitOn)
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M
import System.Environment.XDG.BaseDir
import Control.Exception
import Control.Monad
import System.IO.Error
import Data.Char
import Graphics.Vty.Input.Events (Modifier, Key(KChar,KFun))
import Text.Read
import qualified Data.List.Split as Split

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
parseSection (Ini ini) sec_name parser =
    mapLeft (\s -> "In section \"" ++ sec_name ++ "\": " ++ s) $
    case M.lookup (pack sec_name) ini of
      Just values -> parser values
      Nothing -> parser M.empty

parseEnvironment :: SectionParser (M.HashMap Text Text)
parseEnvironment = Right

data KeyConfig = KeyConfig { globalKeys :: [(KeyCombi, [String])] } -- | a mapping of strings to commands

-- | filepath to user's key config
keyConfigUserPath :: IO FilePath
keyConfigUserPath = getUserConfigFile "caligraph" "keys.ini"

-- | filepath to the default key config
keyConfigDefaultPath :: IO FilePath
keyConfigDefaultPath = getDataFileName "example-config/keys.ini"

parseKeyConfig :: Text -> Either String KeyConfig
parseKeyConfig src = fmap KeyConfig $ do
  ini <- parseIni src
  let sec = maybe [] id $ fmap M.toList $ M.lookup (pack "") (unIni ini)
  forM sec (\(key,value) -> do
      keyParsed <- mapLeft (\_ -> "Invalid key combi " ++ unpack key)
                   (readEither $ unpack key)
      let command = splitOn " " (unpack value)
      return (keyParsed :: KeyCombi, command :: [String]))

data PrettyKey = PrettyKey { prettyKey :: Key } deriving (Eq,Ord)

instance Show PrettyKey where
    show (PrettyKey k) = case k of
        KChar '-' -> "minus"
        KChar ch -> [ch]
        KFun int -> ('F' : show int)
        _ -> drop 1 (show k) -- drop the 'K'

instance Read PrettyKey where
    readsPrec _ str = fmap (\x -> (PrettyKey x, "")) $
            let len = length str in
            if str == "minus"
            then [KChar '-']
            else
                if len == 1
                then [KChar (str !! 0)]
                else
                    case str of
                    ('@':intStr) -> do
                        (keycode,_) <- readsPrec 0 intStr
                        return $ KChar $ chr keycode
                    ('F':intStr) -> do
                        (int,_) <- readsPrec 0 intStr
                        return $ KFun int
                    _ -> do
                        fmap fst $ readsPrec 0 ('K' : str)

data PrettyModifier = PrettyModifier { prettyModifier :: Modifier } deriving (Eq,Ord)

instance Show PrettyModifier where
    show (PrettyModifier m) = drop 1 $ show m -- drop the first M

instance Read PrettyModifier where
    readsPrec k str = do
        (m,r) <- readsPrec k ('M':str)
        return (PrettyModifier m, r)

data KeyCombi = KeyCombi {keyCombi :: ([Modifier], Key)} deriving (Eq,Ord) -- a list of modifiers and a key

instance Show KeyCombi where
    show (KeyCombi (mods, key)) =
        foldl1 (\x y -> x ++ ['-'] ++ y) $
        map (show . PrettyModifier) mods ++ [show $ PrettyKey key]

instance Read KeyCombi where
    readsPrec _ str =
        case reverse $ Split.splitOn "-" str of
            [] -> [] -- no parse
            (keyStr: mods) ->
                fmap (\x -> (KeyCombi x,"")) $ do
                    modsM <- mapM (readsPrec 0) (reverse mods)
                    (key,_) <- readsPrec 0 keyStr
                    return (fmap (prettyModifier . fst) modsM, prettyKey key)

