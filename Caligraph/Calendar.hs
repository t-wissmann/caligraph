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

import System.Directory (getHomeDirectory)
import System.FilePath (takeDirectory, joinPath)

import Control.Monad.Identity
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
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
    , _calConfig :: Conf.CalendarConfig
    }

makeLenses ''RawCalendar

data Calendar = forall stateType eventType.
        Calendar (RawCalendar stateType eventType)

type ConfiguredCalendar
  = IO ()
  -- ^ IO action that the calendar has to
  --   perform when requested data is ready
  -> IO ()
  -- ^ IO action that the calendar can perform when
  --   unrequested data is ready (e.g. some file changed)
  -> IO Calendar
  -- ^ the calendar

type CalendarT m a = StateT Calendar (WriterT [LogLine] m) a
type CalendarM a = CalendarT Identity a

calendarConfig :: Calendar -> Conf.CalendarConfig
calendarConfig (Calendar c) = _calConfig c

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

fromConfig :: Conf.CalendarConfig -> ExceptT String IO ConfiguredCalendar
fromConfig cc = do
    (_,CBR.SomeBackend be) <- except $ maybe (Left $ "No backend named \"" ++ bet ++ "\"") Right $
        find ((==) bet . fst) CBR.backends
    homeDir <- liftIO getHomeDirectory
    (state,wakeUpLoop) <- except $ CB.create be (getOption homeDir)
    return $ \noticeDataReady noticeWakeUp -> do
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
        return $ Calendar $ RawCalendar state be [] args results wakeUpChan False thread cc
    where
        bet = Conf.backendType cc
        getOption :: FilePath -> CB.ConfigRead
        getOption homeDir = CB.ConfigRead {
            CB.configString = (\x ->
                fmap T.unpack $ M.lookup (T.pack x) $ Conf.allSettings cc)
        ,   CB.configFilePath = \x -> do
                relpath_tilde <- fmap T.unpack $ M.lookup (T.pack x) $ Conf.allSettings cc
                let relpath = runIdentity $
                                CU.expandTildeForHome relpath_tilde (return homeDir)
                return $
                  case relpath of
                    ('/':_) -> relpath -- path is already absolute
                    ('.':'/':_) -> -- path is relative
                      if (Conf.configFilePath cc == "")
                      then relpath  -- keep relative
                      else
                      -- interpret relpath relative to the location of the
                      -- calendar config file.
                      let dir = takeDirectory (Conf.configFilePath cc) in
                      joinPath [dir, relpath]
                    _ -> -- do not modify path if unsure
                      relpath
        }

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
    it_src <- zoomBackend (\be -> CB.itemSource be ptr)
    case it_src of
      CB.ExistingFile (path, line) cb -> do
        path' <- liftIO $ CU.expandTilde path
        liftIO $ CU.editFileExternally path' line
        zoomBackendEvent $ CB.Response cb
      CB.NoSource ->
        tell ["Source modification not supported"]

    ---- st <- get
    ---- be <- return (st^.calBackend)
    --zoom calState $ mapStateT liftIO $ CB.editExternally be identifier

addReminder :: Monad m => CB.PartialReminder -> CalendarT m ()
addReminder pr = doCalendar $ zoomBackendEvent $ CB.AddReminder pr


