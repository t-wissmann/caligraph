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
import Caligraph.Cli.Types (LogLine)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Text as T
import Data.List (find)
import Data.Time.Calendar (Day)

import Data.Array
import Data.Traversable

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent

data RawCalendar stateType eventType = RawCalendar
    { _calState :: stateType
    , _calBackend :: CB.Backend stateType eventType
    , _calOpenQueries :: [CB.BackendQuery eventType]
    , _calMVarQuery :: MVar (IO eventType)
    , _calMVarResult :: MVar eventType
    , _calWakeUpChan :: Chan eventType -- channel for sending wakeup events
    , _calWaitingForResult :: Bool
    , _calWorker :: ThreadId
    }

makeLenses ''RawCalendar

data Calendar = forall stateType eventType.
        Calendar (RawCalendar stateType eventType)

type CalendarT m a = StateT Calendar (WriterT [LogLine] m) a
type CalendarM a = CalendarT Identity a

openQueryCount :: Calendar -> Int
openQueryCount (Calendar c) =
  length (c^.calOpenQueries)
  + if (c^.calWaitingForResult) then 1 else 0

zoomBackend :: Monad m => (CB.Backend s q -> CB.BackendM s q a) -> StateT (RawCalendar s q) (WriterT [LogLine] m) a
zoomBackend state_action = do
    be <- use calBackend
    st <- use calState
    let ((r,new_st),new_actions) = runWriter (runStateT (state_action be) st)
    calState .= new_st
    -- first back up entire old queye
    backupQueue <- use calOpenQueries
    calOpenQueries .= []
    -- put all the new items in front of the new queue (in correct order)
    forM_ new_actions (\i ->
        case i of
            CB.BAQuery newQuery ->
              calOpenQueries %= (\oldQueue -> oldQueue ++ [newQuery])
            CB.BAError r -> tell ["Error: " ++ r]
            CB.BALog r -> tell [r])
    -- append the old elements of the previous queue
    calOpenQueries %= flip (++) backupQueue
    return r

zoomBackendEvent :: Monad m => (CB.Event q) -> StateT (RawCalendar s q) (WriterT [LogLine] m) ()
zoomBackendEvent ev = zoomBackend (\be -> CB.handleEvent be ev)

doCalendar :: Monad m => (forall s q.
    StateT (RawCalendar s q) m r) -> StateT Calendar m r
doCalendar computation = do
    Calendar rc <- get
    (r, rc') <- lift $ runStateT computation rc
    put (Calendar rc')
    return r

fromConfig :: IO () -> IO () -> Conf.CalendarConfig -> Either String (IO Calendar)
fromConfig noticeDataReady noticeWakeUp cc = do
    (_,CBR.SomeBackend be) <- maybe (Left $ "No backend named \"" ++ bet ++ "\"") Right $
        find ((==) bet . fst) CBR.backends
    (state,wakeUpLoop) <- CB.create be getOption
    return $ do
        args <- newEmptyMVar
        results <- newEmptyMVar
        wakeUpChan <- newChan
        forkIO $ wakeUpLoop (\event -> do
            noticeWakeUp
            writeChan wakeUpChan event)
        thread <- forkIO $ forever $ do
            action <- takeMVar args
            r <- action
            noticeDataReady
            putMVar results r
        return $ Calendar $ RawCalendar state be [] args results wakeUpChan False thread
    where
        bet = Conf.backendType cc
        getOption :: String -> Maybe String
        getOption x = fmap T.unpack $ M.lookup (T.pack x) $ Conf.allSettings cc

setRangeVisible :: Monad m => (Day,Day) -> CalendarT m ()
setRangeVisible range = doCalendar $ do zoomBackendEvent $ CB.SetRangeVisible range

cachedIncarnations :: Calendar -> (Day,Day) -> CB.Incarnations'
cachedIncarnations (Calendar c) range =
    (CB.cachedIncarnations (_calBackend c) (_calState c) range)

fileQuery :: MonadIO io => CalendarT io ()
fileQuery = doCalendar $ do
    waiting <- use calWaitingForResult
    queue <- use calOpenQueries
    when (not waiting) $
        case queue of
            [] -> return ()
            ((CB.BackendQuery msg action):queue') -> do
                mv <- use calMVarQuery
                liftIO $ putMVar mv action
                calWaitingForResult .= True
                calOpenQueries .= queue'
                tell [msg]

receiveResult :: MonadIO io => CalendarT io ()
receiveResult = doCalendar $ do
    waiting <- use calWaitingForResult
    when waiting $ do
        mv <- use calMVarResult
        res <- liftIO $ takeMVar mv
        calWaitingForResult .= False
        zoomBackendEvent $ CB.Response res

receiveWakeUp :: MonadIO io => CalendarT io ()
receiveWakeUp = doCalendar $ do
    chan <- use calWakeUpChan
    event <- liftIO $ readChan chan
    zoomBackendEvent $ CB.Response event

editExternally :: MonadIO io => Ptr -> CalendarT io ()
editExternally ptr = doCalendar $ do
    be <- use calBackend
    CB.ExistingFile (path, line) cb <- zoomBackend (\be -> CB.itemSource be ptr)
    path' <- liftIO $ CU.expandTilde path
    liftIO $ CU.editFileExternally path' line
    zoomBackendEvent $ CB.Response cb

    ---- st <- get
    ---- be <- return (st^.calBackend)
    --zoom calState $ mapStateT liftIO $ CB.editExternally be identifier

addReminder :: Monad m => CB.PartialReminder -> CalendarT m ()
addReminder pr = doCalendar $ zoomBackendEvent $ CB.AddReminder pr


