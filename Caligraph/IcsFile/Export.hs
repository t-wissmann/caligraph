module Caligraph.IcsFile.Export where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Data.ByteString.Lazy as B
import Data.Default
import Data.Version (Version(Version))
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.ICalendar.Printer
import Text.ICalendar.Types

print :: [CB.Incarnation ()] -> B.ByteString
print incs = printICalendar def $ VCalendar
    { vcProdId = (ProdId (T.pack "caligraph") def)
    , vcVersion = (MaxICalVersion (Version [] []) def)
    , vcScale = def
    , vcMethod = Nothing
    , vcOther = S.empty
    , vcTimeZones = M.empty
    , vcEvents = M.empty
    , vcTodos = M.empty
    , vcJournals = M.empty
    , vcFreeBusys = M.empty
    , vcOtherComps = S.empty
    }

convert_incarnation :: CB.Incarnation () -> VEvent
convert_incarnation inc = VEvent
    { veDTStamp       = undefined -- FIXME
    , veUID           = undefined -- FIXME
    , veClass         = def :: Class -- ^ 'def' = 'Public'
    , veDTStart       = Nothing
    , veCreated       = Nothing
    , veDescription   = Nothing
    , veGeo           = Nothing
    , veLastMod       = Nothing
    , veLocation      = Nothing
    , veOrganizer     = Nothing
    , vePriority      = def
    , veSeq           = def
    , veStatus        = Nothing
    , veSummary       = Nothing
    , veTransp        = def
    , veUrl           = Nothing
    , veRecurId       = Nothing
    , veRRule         = S.empty
    , veDTEndDuration = Nothing
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
