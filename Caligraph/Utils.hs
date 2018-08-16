module Caligraph.Utils where

import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

import System.Environment
import System.Process
import Data.Array as A
import Data.Maybe
import Data.Functor.Identity

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

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

diffTime
    :: (Int,Int)
    -- ^ substract this
    -> (Int,Int)
    -- ^ by this
    -> (Int,Int)
    -- ^ the result in (hours,minutes), where minutes is in [0,59]
    --   if the difference is positive and in [-59,0] otherwise.
diffTime (to_h,to_m) (from_h,from_m) =
    (sgn * (diff_in_min `div` 60), sgn * (diff_in_min `mod` 60))
    where
        diff_in_min' = (to_h * 60 + to_m) - (from_h * 60 + from_m)
        (sgn,diff_in_min) =
            if diff_in_min' < 0
            then (-1, -diff_in_min')
            else (1,   diff_in_min')

editFileExternally :: FilePath -> Int -> IO ()
editFileExternally filepath line = do
    editor <- fmap (fromMaybe "vi") $ lookupEnv "EDITOR"
    callProcess editor ["+" ++ show line, filepath]
    return ()

embed :: Monad m => State s r -> StateT s m r
embed = mapStateT (return . runIdentity)

readOnly :: Monad m => ReaderT s m r -> StateT s m r
readOnly r = do
    s <- get
    lift $ runReaderT r s

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left b) = Left $ f b
mapLeft _ (Right b) = Right b

forState :: (Traversable t, Monad m) => StateT s m r -> StateT (t s) m (t r)
forState prog = do
    values <- get
    (results,states) <- lift $ fmap funzip $ mapM (runStateT prog) values
    put states
    return results
    where
        funzip :: Functor f => f (a,b) -> (f a, f b)
        funzip x = (fmap fst x, fmap snd x)


