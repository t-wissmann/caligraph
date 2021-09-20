{-# LANGUAGE TemplateHaskell #-}

module Caligraph.Headless where

import Caligraph.Backend.Types as CB
import Caligraph.Backend.Utils as CU
import Caligraph.Cli.Types
import Caligraph.Breakpoint
import qualified Caligraph.Calendar as CC
import Caligraph.Cli.DayGrid (getToday)

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Array
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (Day,addDays)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex


import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl ((%=))
import Control.Monad.State
import Control.Monad.Writer.Lazy
import System.Exit (ExitCode)
import Control.Concurrent.Chan
import System.IO (hPutStrLn, stderr)

data HeadlessOptions = HeadlessOptions
    -- { hoFirstDay :: Maybe Day
    -- , hoLastDay :: Maybe Day
    -- }
    { hoFirstDay :: Maybe Day
    , hoLastDay :: Maybe Day
    , hoTitleFilter :: Maybe Regex
    }

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
  liftIO $ forM_ logs (\l -> hPutStrLn stderr $ (T.unpack name) ++ ": " ++ l)
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
    -- liftIO $ hPutStrLn stderr "waiting for response..."
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

main :: [(T.Text,CC.ConfiguredCalendar)] -> HeadlessOptions -> IO ()
main cals opts = do
  state <- initState cals
  -- extract all reminders and print each of them
  ((),state') <- runStateT (extractReminders opts >>= (liftIO . printReminders)) state
  return ()


extractReminders :: HeadlessOptions -> HeadlessT IO [FullReminder]
extractReminders opts = do
  today <- liftIO getToday
  let firstDay = fromMaybe today $ hoFirstDay opts
  let lastDay = fromMaybe today $ hoLastDay opts
  forEachCalendar (CC.setRangeVisible (firstDay,lastDay))
  syncCalendars
  incs <- fmap (map $ fmap $ flip CC.cachedIncarnations (firstDay,lastDay)) $ gets _calendars
  return $ flip concatMap (incs :: [(Text,CB.Incarnations')])
         $ \(name,arr) ->
           map (FullReminder name)
           -- remove the identifier in reminders:
           $ map (fmap (const ()))
           -- concat all days
           $ concat
           -- filter by title
           $ (case (hoTitleFilter opts) of
              Nothing -> id
              Just reg -> map $ filter (isJust . matchRegex reg . CB.title)
           )
           -- remove day
           $ map snd
           -- filter by days in above range
           $ filter (\(d,_) -> d >= firstDay && d <= lastDay)
           -- get all pairs of days and reminders on that days
           $ assocs arr


printReminders
    :: [FullReminder]
    -> IO ()
printReminders = B.putStrLn . encodePretty

data FullReminder = FullReminder
    { frCalendar :: Text
    , frMain :: CB.Incarnation ()
    }

instance ToJSON FullReminder where
    toJSON rem =
        let r = frMain rem in
        object $ map (\(k,v) -> T.pack k .= v) $
        [ (,) "calendar" $ frCalendar rem
        , (,) "title"    $ T.pack $ CB.title r
        , (,) "day"      $ T.pack $ show $ CB.day r
        , (,) "time"     $ showTime $ CB.time r
        , (,) "duration" $ showTime $ CB.duration r
        , (,) "endtime"  $ showTime $ fst endTimeAndDay
        , (,) "endday"   $ T.pack $ maybe "" show $ snd endTimeAndDay
        ]
        where
          showTime :: Maybe (Int,Int) -> Text
          showTime = T.pack . dropWhile ((==) ' ') . maybe "" (CU.showTime ':')
          endTimeAndDay :: (Maybe (Int,Int), Maybe Day)
          endTimeAndDay = pairmaps (fmap fst) (fmap snd) $ do -- in the maybe monad
            let r = frMain rem
            (h1,m1) <- CB.time r
            (h2,m2) <- CB.duration r
            let m_final = (m1 + m2) `mod` 60
            let h_final = h1 + h2 + (m1 + m2) `div` 60
            return ((h_final `mod` 24, m_final), (fromIntegral $ h_final `div` 24) `addDays` CB.day r)
          pairmaps :: (a -> b) -> (a -> c) -> (a -> (b,c))
          pairmaps f g a = (f a, g a)




