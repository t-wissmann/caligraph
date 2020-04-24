
module Caligraph.Backend.Utils where

import Caligraph.Backend.Types

import Caligraph.Utils

import Data.Time.Calendar (Day)
import Data.Time.Calendar (Day,addDays,diffDays)

import Control.Monad.State (get, gets, lift, put,StateT,mapStateT)
import Control.Monad.Reader (Reader,withReader,ReaderT,runReaderT)
import Data.Functor.Identity
import Control.Monad.Writer (tell)

import System.FilePath

import Text.Printf
import Text.ParserCombinators.Parsec
import Data.Ord
import Data.Maybe
import Data.Array
import qualified Data.List as List

showTime :: Char -> (Int,Int) -> String
showTime c (h,m) =
    printf ("%2d" ++[c] ++"%02d" ) h m

lifetimeIntersects
  :: (Day,Day)
  -- ^ closed interval
  -> (Maybe Day, Maybe Day)
  -- ^ possibly open interval
  -> Bool
  -- ^ whether the intervals intersect
lifetimeIntersects (f,t) (ml,mr) =
    (case mr of
      Just r -> f <= r
      Nothing -> True)
    &&
    (case ml of
      Just l -> l <= t
      Nothing -> True)

inLifetime :: Day -> (Maybe Day, Maybe Day) -> Bool
inLifetime d range = lifetimeIntersects (d,d) range

query_items :: Ord i => [Item i] -> (Day,Day) -> Array Day [Incarnation i]
query_items items (from,to)
    = accumArray (flip (:)) [] (from,to)
    -- we need to sort in reverse order,
    -- because we prepend via (:) in accumArray
    $ List.sortBy (comparing Down)
    $ map (\x -> (day x, x))
    $ filter (\i -> (day i) `inLifetime` (Just from, Just to))
    $ concatMap (\i -> incarnations i from to)
    $ filter (lifetimeIntersects (from,to) . lifetime)
    $ items
    where same f x y = f x == f y

read_only :: Monad m => ReaderT s m r -> StateT s m r
read_only computation = do
    s <- get
    lift $ runReaderT computation s

callback :: String -> IO event -> BackendM state event ()
callback txt io_action = tell [BAQuery $ BackendQuery txt io_action]

parseNonNegative :: (Read a, Num a) => GenParser Char st a
parseNonNegative = read <$> many1 digit

tryParseTime
    :: String
    -- ^ a string to parse
    -> Maybe (Int,Int)
    -- ^ maybe a time. the second component is in the range [0,59]
tryParseTime = (.) either2maybe $ flip parse "" $ do
    h <- num
    m <- option 0 (do { char ':' ; num })
    offset <- choice [ string "am" >> return 0
                     , string "pm" >> return 12
                     , return 0
                     ]
    eof
    return (h + offset,m)
    where either2maybe (Left _) = Nothing
          either2maybe (Right x) = Just x
          num = parseNonNegative

-- | try to find a time and duration specification in the given string
-- The first word can consist of a single time or of two time
-- specifications seperated by a squence of '-'. If the first word is neither
-- then the string is returned unchanged. See tryParseTime for valid
-- time sepcifications
parseTimeDuration :: String -> (Maybe (Int,Int), Maybe (Int,Int), String)
parseTimeDuration buf =
    if isJust from_maybe
    then (from_maybe,duration_maybe, dropWhile (== ' ') suffix)
    else (Nothing, Nothing, buf)
    where
        -- split buf in first word and rest
        (duration_str,suffix) = span (/= ' ') buf
        (from_str,to_str') = span (/= '-') duration_str
        to_str = dropWhile (== '-') to_str'
        (from_maybe,duration_maybe) =
            if to_str == ""
            then (tryParseTime from_str, Nothing)
            else maybe (Nothing,Nothing) (\(x,y) -> (Just x, Just y))
                $ do
                from <- tryParseTime from_str
                to <- tryParseTime to_str
                return
                    $ if (fst (to `diffTime` from) >= 0)
                    then (from, to `diffTime` from)
                    else (from, from `diffTime` to)

-- | a wrapper saying that a certain field in the config is mandatory
mandatory :: ConfigRead -> (ConfigRead -> ConfigGetter a) -> String -> Either String a
mandatory cfg getter key =
    case (getter cfg key) of
        Nothing -> Left $ "Mandatory key " ++ key ++ " missing"
        Just x -> Right x
--
-- | a wrapper saying that a certain field in the config is optional
optional :: ConfigRead -> (ConfigRead -> ConfigGetter a) -> String -> a -> Either String a
optional cfg getter key defaultValue =
    case (getter cfg key) of
        Nothing -> Right defaultValue
        Just x -> Right x
