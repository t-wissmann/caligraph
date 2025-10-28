{-# LANGUAGE TemplateHaskell #-}
module Caligraph.ICalendar.Backend where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Caligraph.Utils as CU
import qualified Caligraph.PointerStore as PS

import Data.Either
import Data.Maybe
import Data.Foldable
import Data.Default
import Data.Text.Lazy (Text, pack, unpack)

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Hashable
import qualified Data.Array as A

import System.FilePath
import Control.Monad
import Control.Monad.Writer
import Text.ParserCombinators.Parsec

import qualified Text.ICalendar.Types as ICal
import qualified Text.ICalendar.Parser as ICal
import qualified Data.Map.Strict as Map

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

type VEventKeyBare = (Text, Maybe (Either ICal.Date ICal.DateTime))

-- | ICal.Date and ICal.DateTime are not hashable, so we need a wrapper type
data VEventKey = VEventKey { vEventKey :: VEventKeyBare }
    deriving (Eq,Show,Ord)

instance Hashable VEventKey where
    hash = hash . mapSnd (fmap (fmap show . CU.mapLeft show)) . vEventKey
        where mapSnd f (a,b) = (a, f b)
    hashWithSalt s = hashWithSalt s . mapSnd (fmap (fmap show . CU.mapLeft show)) . vEventKey
        where mapSnd f (a,b) = (a, f b)

data St = St
    { _path :: String -- a filepath with tilde not yet expanded
    , _calendar :: ICal.VCalendar
    , _furtherCalendars :: [ICal.VCalendar] -- further calendars in the same file
    , _idStore :: PS.PointerStore VEventKey
    , _bootup :: Bool
    -- ^ whether we are in the startup phase
    }

makeLenses ''St

data Event =
  CalendarLoaded (Either String ([ICal.VCalendar], [String]))
  | SourceEdited
  | FileChanged Bool
  -- ^ when the file was modified, and whether it still exists

parseConfig :: CB.ConfigRead -> Either String (St, CB.WakeUpLoop Event)
parseConfig cfg = do
  path <- mandatory CB.configFilePath "path"
  let st = St path def [] PS.empty True
  return (st, wakeUpLoop st)
  where
    mandatory = CB.mandatory cfg

wakeUpLoop :: St -> CB.WakeUpLoop Event
wakeUpLoop st reportEvent = do
    CU.watchFile (st^.path) $ \exists ->
      reportEvent $ FileChanged exists

handleEvent :: CB.Event Event -> CB.BackendM St Event ()
handleEvent (CB.SetRangeVisible (from,to)) = return ()
handleEvent (CB.AddReminder pr) = do
  fullpath <- use path
  CB.callback ("Not implemented yet: Adding reminder to " ++ fullpath) $ do
    return SourceEdited

handleEvent (CB.Response (CalendarLoaded cOrError)) = do
  fp <- use path
  case cOrError of
    Right (cals, warnings) -> do
      forM warnings $ \msg -> tell [CB.BAError $ "Warning: " ++ msg]
      case cals of
        [] -> do
            tell [CB.BALog "Loaded: empty calendar file"]
            calendar .= def
        (cal:further) -> do
            calendar .= cal
            forM (Map.keys $ ICal.vcEvents cal) $ \key ->
                zoom idStore $ PS.lookupOrInsert (VEventKey key)
            furtherCalendars .= further
            let eventCount = Map.size (ICal.vcEvents cal)
            tell [CB.BALog $ "Loaded \"" ++ fp ++ "\" with " ++ show eventCount ++ " events"]
    Left error -> tell [CB.BAError $ "Failed to load file: " ++ error]
handleEvent (CB.Response (SourceEdited)) = return ()
handleEvent (CB.Response (FileChanged exists)) = do
      fp <- use path
      if exists
      then tell [CB.BALog $ "File " ++ fp ++ " modified"] >> reloadFile
      else tell [CB.BALog $ "File " ++ fp ++ " removed"]

reloadFile :: CB.BackendM St Event ()
reloadFile = do
  fp <- use path
  CB.callback ("Loading " ++ fp) $ fmap CalendarLoaded $ do
    eitherResult <- ICal.parseICalendarFile def fp
    return eitherResult

vevent2incarnation :: ICal.VEvent -> ICal.DTStart -> a -> CB.Incarnation a
vevent2incarnation event start itemId =
    CB.Incarnation day time duration summary itemId
    where
        projHM :: TimeOfDay -> (Int,Int)
        projHM tod = (todHour tod, todMin tod)
        difftime2HM :: DiffTime -> (Int,Int)
        difftime2HM difft =
            let picosec = diffTimeToPicoseconds difft in
            let tenExp12 = 1000000000000 in
            let mindiff = fromIntegral ((picosec `div` tenExp12) `div` 60) in
            (mindiff `div` 60, mindiff `mod` 60)

        datetime2tuple :: ICal.DateTime -> (Day, Maybe (Int,Int))
        datetime2tuple d =
                case d of
                ICal.FloatingDateTime (LocalTime d t) -> (d, Just $ projHM t)
                ICal.UTCDateTime (UTCTime d t) -> (d, Just $ difftime2HM t)
                ICal.ZonedDateTime (LocalTime d t) zone -> (d, Just $ projHM t)
        (day, time) = case start of
            ICal.DTStartDateTime d _ -> datetime2tuple d
            ICal.DTStartDate d _ -> (ICal.dateValue d, Nothing)
        duration = case ICal.veDTEndDuration event of
                  Nothing -> Nothing
                  Just (Left (ICal.DTEndDate _ _)) -> Nothing
                  Just (Left (ICal.DTEndDateTime endDateTime _)) ->
                    do -- in Maybe
                    has_start_time <- time
                    has_end_time <- snd $ datetime2tuple endDateTime
                    return $ has_end_time `CU.diffTime` has_start_time

                  Just (Right ical_duration) ->
                    case (ICal.durationValue ical_duration) of
                    ICal.DurationDate sign d h m sec -> Just (h, m)
                    ICal.DurationTime sign h m sec  -> Just (h, m)
                    ICal.DurationWeek _ _ -> Nothing
        summary = fromMaybe "" (unpack <$> ICal.summaryValue <$> ICal.veSummary event)


cachedIncarnations :: St -> (Day,Day) -> CB.Incarnations'
cachedIncarnations st (from,to) =
    A.accumArray (flip (:)) [] (from,to)
    $ Map.foldrWithKey' maybeAddEvent []
    $ ICal.vcEvents (_calendar st)
    where
        maybeAddEvent :: VEventKeyBare -> ICal.VEvent -> [(Day,CB.Incarnation')] -> [(Day,CB.Incarnation')]
        maybeAddEvent key ev l =
            case extractEvent (VEventKey key) ev of
            Just x -> (x:l)
            Nothing -> l

        extractEvent :: VEventKey -> ICal.VEvent -> Maybe (Day,CB.Incarnation')
        extractEvent key event = do
            start <- ICal.veDTStart event
            let itemId = PS.lookupUnsafe (_idStore st) key
            let incarn = vevent2incarnation event start itemId
            let day = CB.day incarn
            if from <= day && day <= to
            then return (day, incarn)
            else Nothing

backend :: CB.Backend St Event
backend = CB.Backend
  { CB.create = parseConfig
  , CB.cachedIncarnations = cachedIncarnations
  , CB.itemSource = (\ptr -> do
      return CB.NoSource)
  , CB.handleEvent = (\ev -> do
      handleEvent ev
      inBootup <- use bootup
      when inBootup $ do
        bootup .= False
        reloadFile
      )
  }

