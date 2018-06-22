
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

callback :: IO event -> BackendM state event ()
callback io_action = tell [BackendQuery $ io_action]

