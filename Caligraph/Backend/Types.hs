module Caligraph.Backend.Types where

import Data.Time.Calendar (Day)
import Data.Time.Calendar (Day,addDays,diffDays)

import Text.Printf
import Data.Ord
import Data.Array
import qualified Data.List as List
import Control.Monad.State

data Incarnation = Incarnation
  { day :: Day
  , time :: Maybe (Int,Int)
  , duration :: Maybe (Int,Int)
  , title :: String
  , identifier :: String
  -- , TODO: a timezone
  } deriving (Eq,Show)

instance Ord Incarnation where
  compare i1 i2 =
    either id (const EQ) $ do
      -- sorting is basically lexicographic
      compare_on day i1 i2
      -- but we want Just x to be smaller than Nothing
      compare_on (Down . fmap Down . time) i1 i2
      compare_on duration i1 i2
      compare_on title i1 i2
      compare_on identifier i1 i2
    where compare_on projection v1 v2
            =
            case compare (projection v1) (projection v2) of
            EQ -> Right ()
            LT -> Left LT
            GT -> Left GT


data Item = Item
  { lifetime :: (Maybe Day, Maybe Day)
  -- ^ the interval in which incarnations of this reminder live
  , incarnations :: Day -> Day -> [Incarnation]
  -- ^ all incarnations within a (closed) interval
  }

type Incarnations = Array Day [Incarnation]

data Backend state = Backend
  { query :: (Day,Day) -> State state Incarnations
  -- ^ querying a certain day range, return a list of (cached) incarnations
  , dequeueIO :: state -> Maybe (IO state)
  -- , editEditor :: String -> StateT state IO ()
  -- ^ given the identfier, edit an item externally in an editor
  , create :: (String -> Maybe String) -> Either String state
  -- ^ create a new instance given the config
  }

