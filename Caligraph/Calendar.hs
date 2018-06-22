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

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Control.Concurrent.MVar
import Control.Concurrent

data RawCalendar stateType eventType = RawCalendar
    { _calState :: stateType
    , _calBackend :: CB.Backend stateType eventType
    , _calOpenQueries :: [CB.BackendQuery eventType]
    , _calMVarQuery :: MVar (IO eventType)
    , _calMVarResult :: MVar eventType
    , _calWaitingForResult :: Bool
    , _calWorker :: ThreadId
    }

makeLenses ''RawCalendar

data Calendar = forall stateType eventType.
        Calendar (RawCalendar stateType eventType)

zoomBackend :: Monad m => (CB.Backend s q -> CB.BackendM s q a) -> StateT (RawCalendar s q) m a
zoomBackend state_action = do
    be <- use calBackend
    st <- use calState
    let ((r,new_st),new_queries) = runWriter (runStateT (state_action be) st)
    calState .= new_st
    calOpenQueries %= (++) new_queries
    return r

zoomBackendEvent :: Monad m => (CB.Event q) -> StateT (RawCalendar s q) m ()
zoomBackendEvent ev = zoomBackend (\be -> CB.handleEvent be ev)

doCalendar :: Monad m => (forall s q.
    StateT (RawCalendar s q) m r) -> StateT Calendar m r
doCalendar computation = do
    Calendar rc <- get
    (r, rc') <- lift $ runStateT computation rc
    put (Calendar rc')
    return r

fromConfig :: IO () -> Conf.CalendarConfig -> Either String (IO Calendar)
fromConfig noticeDataReady cc = do
    (_,CBR.SomeBackend be) <- maybe (Left $ "No backend named \"" ++ bet ++ "\"") Right $
        find ((==) bet . fst) CBR.backends
    state <- CB.create be getOption
    return $ do
        args <- newEmptyMVar
        results <- newEmptyMVar
        thread <- forkIO $ forever $ do
            action <- takeMVar args
            r <- action
            noticeDataReady
            putMVar results r
        return $ Calendar $ RawCalendar state be [] args results False thread
    where
        bet = Conf.backendType cc
        getOption :: String -> Maybe String
        getOption x = fmap T.unpack $ M.lookup (T.pack x) $ Conf.allSettings cc

setRangeVisible :: (Day,Day) -> State Calendar ()
setRangeVisible range = doCalendar $ do zoomBackendEvent $ CB.SetRangeVisible range

cachedIncarnations :: Calendar -> (Day,Day) -> CB.Incarnations'
cachedIncarnations (Calendar c) range =
    (CB.cachedIncarnations (_calBackend c) (_calState c) range)

fileQuery :: MonadIO io => StateT Calendar io ()
fileQuery = doCalendar $ do
    waiting <- use calWaitingForResult
    queue <- use calOpenQueries
    unless waiting $ do
        case queue of
            [] -> return ()
            ((CB.BackendQuery action):queue') -> do
                mv <- use calMVarQuery
                liftIO $ putMVar mv action
                calWaitingForResult .= True
                calOpenQueries .= queue'

receiveResult :: MonadIO io => StateT Calendar io ()
receiveResult = doCalendar $ do
    waiting <- use calWaitingForResult
    when waiting $ do
        mv <- use calMVarResult
        res <- liftIO $ takeMVar mv
        calWaitingForResult .= False
        zoomBackendEvent $ CB.Response res

editExternally :: MonadIO io => Ptr -> StateT Calendar io ()
editExternally ptr = doCalendar $ do
    be <- use calBackend
    CB.ExistingFile (path, line) cb <- CU.embed $ zoomBackend (\be -> CB.itemSource be ptr)
    path' <- liftIO $ CU.expandTilde path
    liftIO $ CU.editFileExternally path' line
    CU.embed $ zoomBackendEvent $ CB.Response cb

    ---- st <- get
    ---- be <- return (st^.calBackend)
    --zoom calState $ mapStateT liftIO $ CB.editExternally be identifier

addReminder :: CB.PartialReminder -> State Calendar ()
addReminder pr = doCalendar $ zoomBackendEvent $ CB.AddReminder pr


