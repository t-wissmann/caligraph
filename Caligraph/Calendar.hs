{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Caligraph.Calendar where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Config.Calendars as Conf
import qualified Caligraph.Backend.Registered as CBR
import qualified Data.HashMap.Strict as M
import Caligraph.PointerStore (Ptr)
import qualified Caligraph.Utils as CU

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Text as T
import Data.List (find)
import Data.Time.Calendar (Day)

import Data.Array
import Data.Traversable
import Data.Hashable

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

data RawCalendar stateType itemID queryType = RawCalendar
    { _calState :: stateType
    , _calBackend :: CB.XBackend stateType itemID queryType
    , _calOpenQueries :: [CB.XBackendQuery queryType]
    }

makeLenses ''RawCalendar

data Calendar = forall stateType itemID queryType. (Eq itemID, Hashable itemID) =>
        Calendar (RawCalendar stateType itemID queryType)

zoomBackend :: (CB.XBackend s i q -> CB.XBackendM s q a) -> State (RawCalendar s i q) a
zoomBackend state_action = do
    be <- use calBackend
    st <- use calState
    let ((r,new_st),new_queries) = runWriter (runStateT (state_action be) st)
    calState .= new_st
    calOpenQueries %= (++) new_queries
    return r


doCalendar :: Monad m => (forall s i q. (Eq i, Hashable i) =>
    StateT (RawCalendar s i q) m r) -> StateT Calendar m r
doCalendar computation = do
    Calendar rc <- get
    (r, rc') <- lift $ runStateT computation rc
    put (Calendar rc')
    return r

fromConfig :: Conf.CalendarConfig -> Either String Calendar
fromConfig cc = do
    (_,CBR.SomeBackend be) <- maybe (Left $ "No backend named \"" ++ bet ++ "\"") Right $
        find ((==) bet . fst) CBR.backends
    state <- CB.xcreate be getOption
    return $ Calendar $ RawCalendar state be []
    where
        bet = Conf.backendType cc
        getOption :: String -> Maybe String
        getOption x = fmap T.unpack $ M.lookup (T.pack x) $ Conf.allSettings cc

setRangeVisible :: (Day,Day) -> State Calendar ()
setRangeVisible range = doCalendar $ do zoomBackend $ (\be -> CB.setRangeVisible be range)

cachedIncarnations :: Calendar -> (Day,Day) -> CB.Incarnations'
cachedIncarnations (Calendar c) range =
    (CB.cachedIncarnations (_calBackend c) (_calState c) range)
    --let
    --    lookup x = evalState (PS.lookup x) (_calIdStore c)
    --    incs = 
    --in fmap (map (fmap lookup)) incs
    --  ^     ^    ^
    --  |     |    |
    --  |     |    '--- within an Incarnation
    --  |     |
    --  |     '--- for each Incarnation
    --  |
    --  '--- for each day
    --
--query :: (Day,Day) -> CB.Incarnations'
---- ^ querying a certain day range, return a list of (cached) incarnations
--query dayRange = doCalendar $ query' dayRange
--
--query' :: (Eq i, Hashable i) => (Day,Day) -> State (RawCalendar i a) (CB.Incarnations')
---- ^ querying a certain day range, return a list of (cached) incarnations
--query' dayRange = do
--    be <- use calBackend
--    incs <- zoom calState (CB.query be dayRange)
--    incs' <- mapM (mapM (mapM (\x -> zoom idStore $ PS.lookup x))) incs
--    --        ^     ^      ^
--    --        |     |      |
--    --        |     |      '--- within an Incarnation
--    --        |     |
--    --        |     '--- for each Incarnation
--    --        |
--    --        '--- for each day
--    return (incs':: CB.Incarnations')

dequeueIO :: Calendar -> Maybe (IO Calendar)
dequeueIO (Calendar (RawCalendar st be queries)) = 
    case queries of
        [] -> Nothing
        _ -> Just $ do
            responses <- mapM CB.bqIO queries
            let (((),st'), queries') = runWriter $ flip runStateT st $ mapM_ (CB.handleResponse be) responses
            return (Calendar (RawCalendar st' be queries'))

editExternally :: MonadIO io => Ptr -> StateT Calendar io ()
editExternally ptr = doCalendar $ do
    be <- use calBackend
    CB.ExistingFile (path, line) cb <- CU.embed $ zoomBackend (\be -> CB.itemSource be ptr)
    path' <- liftIO $ CU.expandTilde path
    liftIO $ CU.editFileExternally path' line
    CU.embed $ zoomBackend (\be -> CB.handleResponse be cb)

    ---- st <- get
    ---- be <- return (st^.calBackend)
    --zoom calState $ mapStateT liftIO $ CB.editExternally be identifier

addReminder :: CB.PartialReminder -> State Calendar ()
addReminder pr = doCalendar $ zoomBackend (\be -> CB.xaddReminder be pr)

