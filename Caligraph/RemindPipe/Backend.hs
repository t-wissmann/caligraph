{-# LANGUAGE TemplateHaskell #-}
module Caligraph.RemindPipe.Backend where

import Caligraph.RemindPipe.Types
import qualified Caligraph.Backend.Types as CB

import Control.Monad.State

import Data.Time.Calendar
import Data.Ix (range)
import qualified Data.Array as A
import qualified Data.Map.Strict as M

import System.FilePath

import Lens.Micro.Mtl
import Lens.Micro.TH
import Debug.Trace

-- basically parse the output of  remind -r -s -l file month year

type Month = (Integer,Int)
type Identifier = (FilePath, Int)
data St = St
    { _path :: String
    , _monthCache :: M.Map Month (CB.Incarnations Identifier)
    , _cacheMisses :: [Month]
    -- ^ mapping a month to the full array for all days of that month
    }

makeLenses ''St

nextMonth :: Month -> Month
nextMonth (year,m) =
    (year + (fromIntegral m) `div` 12, 1 + (m `mod` 12))

-- get the first and the last day of the given month
monthRange :: Month -> (Day,Day)
monthRange (year,m) =
    let (year',m') = nextMonth (year,m) in
    ( fromGregorian year m 1
    , addDays (-1) $ fromGregorian year' m' 1
    )


getMonthIncarnations :: Month -> State St (CB.Incarnations Identifier)
getMonthIncarnations month = do
    mc <- use monthCache
    case M.lookup month mc of
        Just m -> return m
        Nothing -> do
            cacheMisses %= (:) month
            return $ A.accumArray const [] (monthRange month) []

query :: (Day,Day) -> State St (CB.Incarnations Identifier)
query (from,to) = do
    monthsWithRems <- forM months_covered getMonthIncarnations
    return
        $ A.array (from,to)
        $ takeWhile (\(d,_) -> d <= to)
        $ dropWhile (\(d,_) -> d < from)
        $ concatMap A.assocs monthsWithRems
    where
        (from_y,from_m,_) = toGregorian from
        (to_y,to_m,_) = toGregorian to
        months_in_between =
            takeWhile (\x -> x < (to_y,to_m))
                $ iterate nextMonth
                $ nextMonth (from_y,from_m)
        months_covered =
            [(from_y,from_m)] ++ months_in_between ++ [(to_y,to_m)]

parseConfig :: (String -> Maybe String) -> Either String St
parseConfig cfg =
    case (cfg "path") of
        Just path -> return $ St path M.empty []
        Nothing -> Left "Mandatory setting 'path' missing"


backend :: CB.Backend Identifier St
backend = CB.Backend
  { CB.query = query
  , CB.dequeueIO = const Nothing
  , CB.editExternally = (\i -> return ())
  , CB.addReminder = (\prem -> return ())
  , CB.create = parseConfig
  }


