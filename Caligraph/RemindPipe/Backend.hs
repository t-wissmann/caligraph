{-# LANGUAGE TemplateHaskell #-}
module Caligraph.RemindPipe.Backend where

import Caligraph.Remind.Backend (reminderTemplate)
import Caligraph.RemindPipe.Types
import Caligraph.RemindPipe.Parser (parseRemOutput)
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import Caligraph.Remind.Types (month_names)
import Caligraph.Utils (expandTilde,editFileExternally)

import Control.Monad.State
import Control.Monad.Writer (tell)

import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock (getCurrentTime, NominalDiffTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone,localDay)
import Data.Ix (range)
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import Caligraph.PointerStore as PS

import Control.Concurrent
import qualified System.FSNotify as FS

import System.FilePath
import System.Process
import System.Exit
import System.Directory (doesFileExist)
import System.FilePath.Posix


import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Debug.Trace

-- basically parse the output of  remind -r -s -l file month year
data St = St
    { _path :: String -- a filepath with tilde not yet expanded
    , _pathNewReminders :: String -- a filepath with tilde not yet expanded
    , _monthCache :: M.Map Month (CB.Incarnations')
    -- ^ mapping a month to the full array for all days of that month
    , _cacheMisses :: M.Map Month Bool
    -- ^ map whether a missing month has already been requested
    , _idStore :: PS.PointerStore SourceLocation
    -- ^ if the config has just been created
    }

makeLenses ''St

data Event =
    MonthData Month [CB.Incarnation SourceLocation] (ExitCode,String)
    | FileSystem Bool FilePath -- some update from the file system, and whether this file currently exists
    | FlushCache
    | Nop -- dummy event
    | MonthRequestDropped Month -- if the month request has been dropped


nextMonth :: Month -> Month
nextMonth (year,m) =
    (year + (fromIntegral m) `div` 12, 1 + (m `mod` 12))

showMonth :: Month -> String
showMonth (y,m) = (show y) ++ "-" ++ (show m)

previousMonth :: Month -> Month
previousMonth (year,m) =
    if m == 1 then (year - 1, 12)
    else (year, m - 1)

-- get the first and the last day of the given month
monthRange :: Month -> (Day,Day)
monthRange (year,m) =
    let (year',m') = nextMonth (year,m) in
    ( fromGregorian year m 1
    , addDays (-1) $ fromGregorian year' m' 1
    )

-- | return a list of months in the given range
monthsCovered :: (Day,Day) -> [Month]
monthsCovered (from,to) =
  map head $ group $
  [(from_y,from_m)] ++ months_in_between ++ [(to_y,to_m)]
  where
    (from_y,from_m,_) = toGregorian from
    (to_y,to_m,_) = toGregorian to
    months_in_between =
      takeWhile (\x -> x < (to_y,to_m))
          $ iterate nextMonth
          $ nextMonth (from_y,from_m)

cachedIncarnations :: St -> (Day,Day) -> CB.Incarnations'
cachedIncarnations st (from,to) =
  A.array (from,to)
  $ takeWhile (\(d,_) -> d <= to)
  $ dropWhile (\(d,_) -> d < from)
  $ flip concatMap (monthsCovered (from,to))
  $ (\m ->
     fromMaybe (map (flip (,) []) $ range $ monthRange m)
     (fmap A.assocs (M.lookup m (_monthCache st)))
     )

wakeUpLoop :: St -> CB.WakeUpLoop Event
wakeUpLoop st reportEvent = do
    filepath <- liftIO $ expandTilde (st^.path)
    FS.withManagerConf conf $ \mgr -> do
        FS.watchDir
            mgr
            (dropFileName filepath)
            (isAbout filepath)
            (\event -> do
                -- after an event occoured, all further events
                -- within the next debounceTime seconds are ignored.
                -- thus we need to wait for this time to reconstruct
                -- the possibly last event within this time period
                threadDelay (1000 * fromInteger debounceTimeMilliSecs)
                exists <- doesFileExist (FS.eventPath event)
                reportEvent $ FileSystem exists (FS.eventPath event))
        forever $ threadDelay (10 * 1000 * 1000) -- wait for 10 seconds
    where
      debounceTimeMilliSecs = 100 -- additional waiting time
      -- the config collapses all events within 0.1 seconds
      -- we do this kind of debounce, because programes like vim create three events
      -- (remove, add, modifiy - in this order)
      conf = FS.WatchConfig (FS.Debounce $ (fromInteger debounceTimeMilliSecs) / 1000) 1000 False
      isAbout filepath e = case e of
        FS.Unknown _ _ _->
            -- don't react to unknown events (we never ever want to react to read
            -- events for example...
            False
        _ -> takeFileName (FS.eventPath e) == takeFileName filepath

parseConfig :: (String -> Maybe String) -> Either String (St, CB.WakeUpLoop Event)
parseConfig cfg =
    case (cfg "path") of
        Just path ->
            let path_nr = fromMaybe path (cfg "path_append") in
            let state = St path path_nr M.empty M.empty PS.empty in
            return (state, wakeUpLoop state)
        Nothing -> Left "Mandatory setting 'path' missing"

requestMonth :: FilePath -> Month -> IO Event
requestMonth tilde_path (y,month) =
  if y < 1990 || y > 2075 then
    return $ MonthData (y,month) [] (ExitFailure 1, "remind only supports years from 1990 to 2075") 
  else do
    filepath <- liftIO $ expandTilde tilde_path
    let mon_name = month_names !! (month-1)
    let rem = "remind"
    let rem_args = ["-r", "-s", "-l", filepath, mon_name, show y]
    (exitCode,raw_output,raw_error) <- liftIO $ readProcessWithExitCode rem rem_args ""
    let days_in_month = parseRemOutput raw_output
    return $ MonthData (y,month) (map snd days_in_month) (exitCode,raw_error)

handleEvent :: CB.Event Event -> CB.BackendM St Event ()
handleEvent (CB.SetRangeVisible days) = do
  mc <- use monthCache
  misses <- use cacheMisses
  forM_ (monthsCovered days) $ \m -> do
    when (not (m `M.member` mc) && not (m `M.member` misses)) $
      cacheMisses %= M.insert m False

handleEvent (CB.AddReminder pr) = do
  tilde_path <- use pathNewReminders
  CB.callback ("Adding reminder to " ++ tilde_path) $ do
      path' <- expandTilde tilde_path
      appendFile path' $ reminderTemplate pr
      -- nothing to do, because the file watcher will flush the cache
      return Nop

handleEvent (CB.Response (FlushCache)) = do
  tell [CB.BALog $ "Flushing cache"]
  mc <- use monthCache
  forM_ (M.keys mc) $ \m -> do
      cacheMisses %= M.insert m False

handleEvent (CB.Response (Nop)) = return ()

handleEvent (CB.Response (FileSystem exists filepath)) =
      if exists
      then tell [CB.BALog $ "File " ++ filepath ++ " modified"]
           >> handleEvent (CB.Response (FlushCache))
      else tell [CB.BALog $ "File " ++ filepath ++ " removed"]

handleEvent (CB.Response (MonthData m days (exitCode,stderr))) = do
  days' <- flip mapM days $ \inc -> do
    ptr <- zoom idStore $ PS.lookupOrInsert (CB.itemId inc)
    return (CB.day inc, inc { CB.itemId = ptr})
  cacheMisses %= M.delete m
  monthCache %= M.insert m ((A.accumArray (flip (:)) [] (monthRange m) days') :: CB.Incarnations')
  when ("" /= stderr) $ tell [CB.BAError stderr]
  return ()

handleEvent (CB.Response (MonthRequestDropped m)) = do
  tell [CB.BALog $ "Request for month " ++ showMonth m ++ " dropped."]
  cacheMisses %= M.delete m

requestMissingMonths :: CB.BackendM St Event ()
requestMissingMonths = do
  tilde_path <- use path
  cm <- use cacheMisses
  cm' <- flip M.traverseWithKey cm (\m v -> do
    -- if v is not True, then we don't have a request for it yet
    unless v $
        CB.callbackDroppable
          ("Requesting month " ++ showMonth m)
          (MonthRequestDropped m)
          (requestMonth tilde_path m)
    return True)
  cacheMisses .= cm'


backend :: CB.Backend St Event
backend = CB.Backend
  { CB.create = parseConfig
  , CB.cachedIncarnations = cachedIncarnations
  , CB.itemSource = (\ptr -> do
    location <- zoom idStore $ PS.resolve ptr
    return $ CB.ExistingFile location Nop)
  , CB.handleEvent = (\ev -> do
      handleEvent ev
      requestMissingMonths
      )
  }

