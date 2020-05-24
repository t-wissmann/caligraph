{-# LANGUAGE TemplateHaskell #-}

module Caligraph.Headless where

import Caligraph.Backend.Types as CB
import Caligraph.Cli.Types
import Caligraph.Breakpoint
import qualified Caligraph.Calendar as CC
import Caligraph.Cli.DayGrid (getToday)

import Data.Array
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (Day)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Control.Monad.State
import Control.Monad.Writer.Lazy
import System.Exit (ExitCode)
import Control.Concurrent.Chan

data HeadlessEvent =
  CalendarProg Int (CC.CalendarT IO ())

data HeadlessState = HeadlessState
    { _calendars :: [(Text,CC.Calendar)]
    , _eventChan :: Chan HeadlessEvent
    }

makeLenses ''HeadlessState

type HeadlessT m a = StateT HeadlessState m a

forCalendar :: MonadIO io => Int -> CC.CalendarT io a -> HeadlessT io a
forCalendar idx prog = do
  (name,c) <- gets (flip (!!) idx . _calendars)
  ((a, c'),logs) <- lift $ runWriterT (runStateT prog c)
  -- TODO: save c' back and print logs
  calendars %= map (updateCal c') . zip [0..]
  liftIO $ forM_ logs (\l -> putStrLn $ (T.unpack name) ++ ": " ++ l)
  return a
  where updateCal c' (i,(n,c)) =
          if i == idx then (n,c') else (n,c)

forEachCalendar :: MonadIO io => CC.CalendarT io a -> HeadlessT io [a]
forEachCalendar prog = do
  num <- gets (length . _calendars)
  forM [0..num - 1] (\i -> forCalendar i prog)

syncCalendars :: HeadlessT IO ()
syncCalendars = do
  cals <- gets _calendars
  openQueries <- fmap sum $ forEachCalendar (CC.openQueryCount <$> get)
  when (openQueries > 0) $ do
    -- liftIO $ putStrLn "waiting for response..."
    forEachCalendar CC.fileQuery
    chan <- gets _eventChan
    CalendarProg idx prog <- liftIO $ readChan chan
    () <- forCalendar idx prog
    syncCalendars

initState :: [(T.Text,CC.ConfiguredCalendar)] -> IO HeadlessState
initState cals = do
  chan <- newChan
  cals' <- forM (zip [0..] cals) $ \(idx, (name, c)) -> do
    let onDataReady = writeChan chan (CalendarProg idx (CC.receiveResult >> CC.fileQuery))
    let onWakeUp = writeChan chan (CalendarProg idx (CC.receiveWakeUp >> CC.fileQuery))
    c' <- c onDataReady onWakeUp
    return (name, c')
  return $ HeadlessState cals' chan

main :: [(T.Text,CC.ConfiguredCalendar)] -> IO ()
main cals = do
  state <- initState cals
  ((),state') <- runStateT showToday state
  return ()


showToday :: HeadlessT IO ()
showToday = do
  day <- liftIO getToday
  forEachCalendar (CC.setRangeVisible (day,day))
  syncCalendars
  incs <- fmap (map $ fmap $ flip CC.cachedIncarnations (day,day)) $ gets _calendars
  liftIO $ forM_ (incs :: [(Text,CB.Incarnations')]) $ \(name,arr) -> do
    let incsOfTheDay = map (fmap (const ())) $ (arr ! day)
    putStrLn (T.unpack name ++ ":")
    forM incsOfTheDay (putStrLn . (++) "  " . show)
    return ()
