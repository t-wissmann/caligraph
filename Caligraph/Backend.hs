
module Caligraph.Backend where

import Data.Time.Calendar (Day)
import Data.Time.Calendar (Day,addDays,diffDays)

import Text.Printf
import Data.Array
import qualified Data.List as List

data Incarnation = Incarnation
  { day :: Day
  , time :: Maybe (Int,Int)
  , duration :: Maybe (Int,Int)
  , title :: String
  -- , TODO: a timezone
  } deriving (Eq,Show,Ord)

showTime :: (Int,Int) -> String
showTime (h,m) =
    printf "%2d:%02d" h m

data Item = Item
  { lifetime :: (Maybe Day, Maybe Day)
  -- ^ the interval in which incarnations of this reminder live
  , incarnations :: Day -> Day -> [Incarnation]
  -- ^ all incarnations within a (closed) interval
  }

data Backend = Backend
  { items :: [Item]
  -- , dataReady :: IO Bool
  -- , kk
  }

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

query :: Backend -> (Day,Day) -> Array Day [Incarnation]
query be (from,to)
    = accumArray (flip (:)) [] (from,to)
    $ map (\x -> (day x, x))
    $ filter (\i -> (day i) `inLifetime` (Just from, Just to))
    $ concatMap (\i -> incarnations i from to)
    $ filter (lifetimeIntersects (from,to) . lifetime)
    $ items be
    where same f x y = f x == f y


