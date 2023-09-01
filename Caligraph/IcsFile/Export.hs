module Caligraph.IcsFile.Export where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Data.ByteString.Lazy as B
import Data.Default
import Data.Version (Version(Version))
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.LocalTime

import Text.ICalendar.Printer
import Text.ICalendar.Types

data Metadata = Metadata
    { currentTime :: UTCTime
    }

init_metadata :: IO Metadata
init_metadata = Metadata <$> getCurrentTime

print :: Metadata -> [CB.Incarnation ()] -> B.ByteString
print meta incs = printICalendar def $ VCalendar
    { vcProdId = (ProdId (T.pack "-//caligraph//thorsten-wissmann.de") def)
    , vcVersion = (MaxICalVersion (Version [2, 0] []) def)
    , vcScale = def
    , vcMethod = Nothing
    , vcOther = S.empty
    , vcTimeZones = M.empty
    , vcEvents = M.fromList $ do
        (index,inc) <- zip [0..] incs
        return $ extract_vevent_keys
               $ convert_incarnation meta index inc
    , vcTodos = M.empty
    , vcJournals = M.empty
    , vcFreeBusys = M.empty
    , vcOtherComps = S.empty
    }

extract_vevent_keys :: VEvent -> ((T.Text, Maybe (Either Date DateTime)), VEvent)
extract_vevent_keys event = ((uidValue $ veUID event, Nothing), event)

convert_incarnation :: Metadata -> Int -> CB.Incarnation () -> VEvent
convert_incarnation meta index inc = VEvent
    { veDTStamp       = DTStamp (currentTime meta) def -- FIXME
    , veUID           = UID (T.pack uid) def -- FIXME
    , veClass         = def :: Class -- ^ 'def' = 'Public'
    , veDTStart       = Just $
        case (CB.time inc) of
        Nothing ->
            DTStartDate (Date $ CB.day inc) def
        Just (h,m) ->
            DTStartDateTime (FloatingDateTime $ LocalTime (CB.day inc) (TimeOfDay h m 0)) def
    , veCreated       = Nothing
    , veDescription   = Nothing
    , veGeo           = Nothing
    , veLastMod       = Nothing
    , veLocation      = Nothing
    , veOrganizer     = Nothing
    , vePriority      = def
    , veSeq           = def
    , veStatus        = Nothing
    , veSummary       = Just (Summary (T.pack $ CB.title inc) Nothing Nothing def)
    , veTransp        = def
    , veUrl           = Nothing
    , veRecurId       = Nothing
    , veRRule         = S.empty
    , veDTEndDuration =
        case (CB.duration inc) of
        Nothing -> Nothing
        Just (h,m) -> Just (Right $ DurationProp (DurationTime def h m 0) def)
    , veAttach        = S.empty
    , veAttendee      = S.empty
    , veCategories    = S.empty
    , veComment       = S.empty
    , veContact       = S.empty
    , veExDate        = S.empty
    , veRStatus       = S.empty
    , veRelated       = S.empty
    , veResources     = S.empty
    , veRDate         = S.empty
    , veAlarms        = S.empty
    , veOther         = S.empty
    }
    where
        uid = "item" ++ show index
