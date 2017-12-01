
module Caligraph.Backend.Utils where

import Caligraph.Backend.Types

import Data.Time.Calendar (Day)
import Data.Time.Calendar (Day,addDays,diffDays)

import Control.Monad.State(get)

import Text.Printf
import Data.Ord
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

query_items :: [Item] -> (Day,Day) -> Array Day [Incarnation]
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

static_backend
    :: ((String -> Maybe String) -> Either String config)
    -- ^ the config loader
    -> (config -> IO [Item])
    -- ^ initialization procedure
    -> Backend (Either config [Item])
static_backend configparser initializer = Backend
    { query = (\dayRange -> do
            st <- get
            return $ case st of
                Left _ -> query_items [] dayRange
                Right items -> query_items items dayRange)
    , dequeueIO = (\st ->
            case st of
                Left config -> Just $ fmap Right $ initializer config
                Right _ -> Nothing)
    , create = fmap Left . configparser
    }

