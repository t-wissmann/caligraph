
module Caligraph.Backend where

import Data.Time.Calendar (Day)
import Data.Time.Calendar (Day,addDays,diffDays)

data Incarnation = Incarnation
  { day :: Day
  , time :: Maybe (Int,Int)
  , duration :: Maybe (Int,Int)
  , title :: String
  -- , TODO: a timezone
  } deriving (Eq,Show)

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

inLifetime :: Day -> (Maybe Day, Maybe Day) -> Bool
inLifetime d (ml,mr) =
    (case mr of
      Just r -> d <= r
      Nothing -> True)
    &&
    (case ml of
      Just l -> l <= d
      Nothing -> True)

query :: Backend -> Day -> [Item]
query be day =
    filter (inLifetime day . lifetime) $ items be


