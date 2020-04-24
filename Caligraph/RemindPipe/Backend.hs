{-# LANGUAGE TemplateHaskell #-}
module Caligraph.RemindPipe.Backend where

import Caligraph.Remind.Backend (reminderTemplate)
import Caligraph.RemindPipe.Types
import Caligraph.RemindPipe.Parser (parseRemOutput)
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Caligraph.Utils as CU
import Caligraph.Remind.Types (month_names)
import Caligraph.Utils (editFileExternally)

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

type Year = Integer

-- basically parse the output of  remind -r -s -l file month year
data St = St
    { _path :: String -- a filepath with tilde not yet expanded
    , _pathNewReminders :: String -- a filepath with tilde not yet expanded
    , _yearCache :: M.Map Year (CB.Incarnations')
    -- ^ mapping a year to the full array for all incarnations
    , _cacheMisses :: M.Map Year Bool
    -- ^ map whether a missing year has already been requested
    , _idStore :: PS.PointerStore SourceLocation
    -- ^ if the config has just been created
    }

makeLenses ''St

data Event =
    YearData Integer [CB.Incarnation SourceLocation] (ExitCode,String)
    | FileSystem Bool FilePath -- some update from the file system, and whether this file currently exists
    | FlushCache
    | Nop -- dummy event


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
--
-- get the first and the last day of the given year
yearRange :: Year -> (Day,Day)
yearRange year = (fromGregorian year 1 1, fromGregorian year 12 31)

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

-- | return a list of years in the given range
yearsCovered :: (Day,Day) -> [Year]
yearsCovered (from,to) = [from_y..to_y]
  where
    (from_y,_,_) = toGregorian from
    (to_y,_,_) = toGregorian to


cachedIncarnations :: St -> (Day,Day) -> CB.Incarnations'
cachedIncarnations st (from,to) =
  A.array (from,to)
  $ takeWhile (\(d,_) -> d <= to)
  $ dropWhile (\(d,_) -> d < from)
  $ flip concatMap (yearsCovered (from,to))
  $ (\y ->
     fromMaybe (map (flip (,) []) $ range $ yearRange y)
     (fmap A.assocs (M.lookup y (_yearCache st)))
     )

wakeUpLoop :: St -> CB.WakeUpLoop Event
wakeUpLoop st reportEvent = do
    CU.watchFile (st^.path) (\exists -> reportEvent $ FileSystem exists (st^.path))

parseConfig :: CB.ConfigRead -> Either String (St, CB.WakeUpLoop Event)
parseConfig cfg = do
    path <- mandatory CB.configFilePath "path"
    path_nr <- optional CB.configFilePath "path_append" path
    let state = St path path_nr M.empty M.empty PS.empty
    return (state, wakeUpLoop state)
    where
      mandatory = CB.mandatory cfg
      optional = CB.optional cfg

requestYear :: FilePath -> Integer -> IO Event
requestYear filepath (y) =
  if y < 1990 || y > 2075 then
    let msg = "remind only supports years from 1990 to 2075" in
    return $ YearData y [] (ExitFailure 1, msg)
  else do
    let rem = "remind"
    -- every year covers at most 54 weeks: 52 weeks entirely in the year,
    -- one week with the year before, and one week with the next year
    let rem_args = ["-r", "-s+55", "-l", filepath, "Jan", show y]
    (exitCode,raw_output,raw_error) <- liftIO $ readProcessWithExitCode rem rem_args ""
    let printedIncarnations = parseRemOutput raw_output
    let getYear = (\(x,_,_) -> x) . toGregorian
    let corretYear = (\(day,incarn) ->  y == getYear day)
    let incarnationsInYear = filter corretYear printedIncarnations
    return $ YearData y (map snd incarnationsInYear) (exitCode,raw_error)

handleEvent :: CB.Event Event -> CB.BackendM St Event ()
handleEvent (CB.SetRangeVisible days) = do
  yc <- use yearCache
  misses <- use cacheMisses
  forM_ (yearsCovered days) $ \y -> do
    when (not (y `M.member` yc) && not (y `M.member` misses)) $
      cacheMisses %= M.insert y False

handleEvent (CB.AddReminder pr) = do
  path' <- use pathNewReminders
  CB.callback ("Adding reminder to " ++ path') $ do
      appendFile path' $ reminderTemplate pr
      -- nothing to do, because the file watcher will flush the cache
      return Nop

handleEvent (CB.Response (FlushCache)) = do
  tell [CB.BALog $ "Flushing cache"]
  yc <- use yearCache
  forM_ (M.keys yc) $ \y -> do
      cacheMisses %= M.insert y False

handleEvent (CB.Response (Nop)) = return ()

handleEvent (CB.Response (FileSystem exists filepath)) =
      if exists
      then tell [CB.BALog $ "File " ++ filepath ++ " modified"]
           >> handleEvent (CB.Response (FlushCache))
      else tell [CB.BALog $ "File " ++ filepath ++ " removed"]

handleEvent (CB.Response (YearData y days (exitCode,stderr))) = do
  days' <- flip mapM days $ \inc -> do
    ptr <- zoom idStore $ PS.lookupOrInsert (CB.itemId inc)
    return (CB.day inc, inc { CB.itemId = ptr})
  cacheMisses %= M.delete y
  yearCache %= M.insert y ((A.accumArray (flip (:)) [] (yearRange y) days') :: CB.Incarnations')
  when ("" /= stderr) $ tell [CB.BAError stderr]
  return ()

requestMissingYears :: CB.BackendM St Event ()
requestMissingYears = do
  path' <- use path
  cm <- use cacheMisses
  cm' <- flip M.traverseWithKey cm (\y v -> do
    -- if v is not True, then we don't have a request for it yet
    unless v $
        CB.callback ("Requesting year " ++ show y) $
            requestYear path' y
    -- we set the value of this year to True
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
      requestMissingYears
      )
  }

