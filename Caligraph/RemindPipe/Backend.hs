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
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone,localDay)
import Data.Ix (range)
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import Caligraph.PointerStore as PS

import System.FilePath
import System.Process
import System.Exit

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
    | FlushCache


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


parseConfig :: (String -> Maybe String) -> Either String St
parseConfig cfg =
    case (cfg "path") of
        Just path ->
            let path_nr = fromMaybe path (cfg "path_append") in
            return $ St path path_nr M.empty M.empty PS.empty
        Nothing -> Left "Mandatory setting 'path' missing"

requestMonth :: FilePath -> Month -> IO Event
requestMonth tilde_path (y,month) = do
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
      return FlushCache

handleEvent (CB.Response (FlushCache)) = do
  mc <- use monthCache
  forM_ (M.keys mc) $ \m -> do
      cacheMisses %= M.insert m False

handleEvent (CB.Response (MonthData m days (exitCode,stderr))) = do
  days' <- flip mapM days $ \inc -> do
    ptr <- zoom idStore $ PS.lookupOrInsert (CB.itemId inc)
    return (CB.day inc, inc { CB.itemId = ptr})
  cacheMisses %= M.delete m
  monthCache %= M.insert m ((A.accumArray (flip (:)) [] (monthRange m) days') :: CB.Incarnations')
  when ("" /= stderr) $ tell [CB.BAError stderr]
  return ()

requestMissingMonths :: CB.BackendM St Event ()
requestMissingMonths = do
  tilde_path <- use path
  cm <- use cacheMisses
  cm' <- flip M.traverseWithKey cm (\m v -> do
    -- if v is not True, then we don't have a request for it yet
    unless v $
        CB.callback ("Requesting month " ++ showMonth m) $
            requestMonth tilde_path m
    return True)
  cacheMisses .= cm'


backend :: CB.Backend St Event
backend = CB.Backend
  { CB.create = parseConfig
  , CB.cachedIncarnations = cachedIncarnations
  , CB.itemSource = (\ptr -> do
    location <- zoom idStore $ PS.resolve ptr
    return $ CB.ExistingFile location FlushCache)
  , CB.handleEvent = (\ev -> do
      handleEvent ev
      requestMissingMonths
      )
  }

