{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Caligraph.Calendar where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Config.Calendars as Conf
import qualified Caligraph.Backend.Registered as CBR
import qualified Data.HashMap.Strict as M
import qualified Caligraph.PointerStore as PS

import Control.Monad.State
import qualified Data.Text as T
import Data.List (find)
import Data.Time.Calendar (Day)

import Data.Array
import Data.Traversable
import Data.Hashable

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

data RawCalendar identType stateType = RawCalendar
    { _calState :: stateType
    , _calBackend :: CB.Backend identType stateType
    , _idStore :: PS.PointerStore identType
    }

makeLenses ''RawCalendar

data Calendar = forall identType stateType.
    (Eq identType, Hashable identType) =>
        Calendar (RawCalendar identType stateType)

doCalendar :: (forall i a. (Eq i, Hashable i) =>
    State (RawCalendar i a) r) -> State Calendar r
doCalendar computation = do
    Calendar rc <- get
    let (r, rc') = runState computation rc
    put (Calendar rc')
    return r

fromConfig :: Conf.CalendarConfig -> Either String Calendar
fromConfig cc = do
    (_,CBR.SomeBackend be) <- maybe (Left $ "No backend named \"" ++ bet ++ "\"") Right $
        find ((==) bet . fst) CBR.backends
    state <- CB.create be getOption
    return $ Calendar $ RawCalendar state be PS.empty
    where
        bet = Conf.backendType cc
        getOption :: String -> Maybe String
        getOption x = fmap T.unpack $ M.lookup (T.pack x) $ Conf.allSettings cc


query :: (Day,Day) -> State Calendar CB.Incarnations'
-- ^ querying a certain day range, return a list of (cached) incarnations
query dayRange = doCalendar $ query' dayRange

query' :: (Eq i, Hashable i) => (Day,Day) -> State (RawCalendar i a) (CB.Incarnations')
-- ^ querying a certain day range, return a list of (cached) incarnations
query' dayRange = do
    be <- use calBackend
    incs <- zoom calState (CB.query be dayRange)
    incs' <- mapM (mapM (mapM (\x -> zoom idStore $ PS.lookup x))) incs
    --        ^     ^      ^
    --        |     |      |
    --        |     |      '--- within an Incarnation
    --        |     |
    --        |     '--- for each Incarnation
    --        |
    --        '--- for each day
    return (incs':: CB.Incarnations')

dequeueIO :: Calendar -> Maybe (IO Calendar)
dequeueIO (Calendar (RawCalendar st be istore)) = do
    io_action <- CB.dequeueIO be st
    return $ do
        st' <- io_action
        return $ Calendar $ RawCalendar st' be istore



