module Caligraph.Utils where

import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

import System.Environment
import System.Process
import Data.Array as A
import Data.Maybe

lastSafe :: [a] -> Maybe a
lastSafe = f Nothing
  where f :: Maybe a -> [a] -> Maybe a
        f _   (x:xs) = f (Just x) xs
        f acc [] = acc

safeArray :: Ix i => A.Array i e -> i -> Maybe e
safeArray arr i =
  if (f <= i && i <= t)
  then Just (arr ! i)
  else Nothing
  where
    (f,t) = A.bounds arr

expandTilde :: FilePath -> IO FilePath
expandTilde s =
    case s of
     ['~'] -> getHomeDirectory
     ('~':'/':tl) -> do
        home <- getHomeDirectory
        return $ joinPath [home, tl]
     x -> return x

editFileExternally :: FilePath -> Int -> IO ()
editFileExternally filepath line = do
    editor <- fmap (fromMaybe "vi") $ lookupEnv "EDITOR"
    callProcess editor ["+" ++ show line, filepath]
    return ()

