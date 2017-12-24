
module Caligraph.Remind.Backend where

import Caligraph.Remind.Parser
import Caligraph.Remind.Types
import Caligraph.Utils

import Data.Either
import Data.Maybe
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import Data.Time.Calendar (Day,addDays,diffDays,fromGregorian,gregorianMonthLength)

import Data.Hashable

data Config = Config FilePath
type ItemID = (String,Int) -- filepath, linenumber
type St = (Config, Maybe [CB.Item ItemID])

algorithm :: itemid -> REM -> CB.Item itemid
algorithm itemid (REM args msg) =
   CB.Item
  { CB.lifetime =
        repetitionLifeTime args startDatePartial
  , CB.incarnations = incs
  , CB.identifier = itemid
  }
  where
    startDatePartial =
      flatPartialDate $ findRemArg DateSpec args
    incs f t =
        [ CB.Incarnation
            d
            (findRemArg AT args)
            (findRemArg DURATION args)
            msg
            itemid
        | d <- incarnationDays args startDatePartial f t
        ]

incarnationDays
  :: [RemArg]
  -- ^ the reminder
  -> PartialDate
  -- ^ the (possibly partial) start date
  -> Day
  -- ^ the first day of the interval of interest
  -> Day
  -- ^ the last day of the interval of interest
  -> [Day]
  -- ^ the days within the interval on which the reminder occurs
incarnationDays args startDatePartial f t =
  case isFullDate startDatePartial of
    Nothing -> incarnationsPartial args startDatePartial f t
    Just startDate ->
       if t < startDate
       then [] -- If the reminder is in the future
       else
         case (findRemArg Repeat args) of
            Just r ->
              incarnationsRepeat args startDate r f t
            Nothing ->
              -- no repetition
              [ startDate ]

-- | on which days does a given reminder with a partial date occur?
incarnationsPartial
  :: [RemArg]
  -- ^ the reminder
  -> PartialDate
  -- ^ the properly partial trigger date
  -> Day
  -- ^ the first day of the interval of interest
  -> Day
  -- ^ the last day of the interval of interest
  -> [Day]
  -- ^ the days within the interval on which the reminder occurs
incarnationsPartial args pdate f t =
  []


-- | on which days does a given reminder using Repeat occur?
incarnationsRepeat
  :: [RemArg]
  -- ^ the reminder
  -> Day
  -- ^ the full DateSpec of the reminder
  -> Int
  -- ^ the Repeat of the reminder
  -> Day
  -- ^ the first day of the interval of interest
  -> Day
  -- ^ the last day of the interval of interest
  -> [Day]
  -- ^ the days within the interval on which the reminder occurs
incarnationsRepeat args firstDay repeat f t =
    map (flip addDays firstDay) $ map ((*) $ toInteger repeat) [rep_from..rep_to]
        where
           day2repetitionIdx :: Fractional a => Day -> a
           day2repetitionIdx d = (fromIntegral $ d `diffDays` firstDay)
                               / (fromIntegral repeat)
           -- the index of the first repetition within in the interval
           rep_start_day = max f $ fromMaybe f $ findRemArg From args
           rep_from = max 0 (ceiling $ day2repetitionIdx rep_start_day)
           rep_end_day = min t $ fromMaybe t $ findRemArg Until args
           -- the index of the last repetition within in the interval
           rep_to = max 0 (floor $ day2repetitionIdx rep_end_day)

repetitionLifeTime :: [RemArg] -> PartialDate -> (Maybe Day,Maybe Day)
repetitionLifeTime args pdate =
  case isFullDate pdate of
    Nothing -> partialDateLifeTime pdate
    Just startDate ->
      case findRemArg Repeat args of
        Just r ->
          ( Just $ fromMaybe startDate $ findRemArg From args
          , findRemArg Until args)
        Nothing -> (Just startDate, Just startDate)

partialDateLifeTime :: PartialDate -> (Maybe Day,Maybe Day)
partialDateLifeTime pdate =
    case pdate of
     PartialDate pd pm (Just year) ->
        ( Just $ fromGregorian year month_begin day_begin
        , Just $ fromGregorian year month_end day_end)
        where
            month_begin = maybe 1 id pm
            month_end = maybe 12 id pm
            day_begin = maybe 1 id pd
            day_end = maybe (gregorianMonthLength year month_end) id pd
     PartialDate pd pm Nothing ->
        ( Nothing
        , Nothing)

parseConfig :: (String -> Maybe String) -> Either String Config
parseConfig cfg =
    case (cfg "path") of
        Just path -> return $ Config path
        Nothing -> Left "Mandatory setting 'path' missing"

load :: Config -> IO [CB.Item ItemID]
load (Config path) = do
  path' <- expandTilde path
  reminders <- parseFile path'
  return
    $ map (uncurry algorithm)
    $ map (\(i,r) -> ((path',i),r))
    $ mapMaybe isRem
    $ rights reminders
  where
    isRem :: (i,RFLine) -> Maybe (i,REM)
    isRem (i,Rem r) = Just (i,r)
    isRem _ = Nothing

reminderTemplate :: CB.PartialReminder -> Reader Config (FilePath, String)
reminderTemplate prem = do
    Config path <- ask
    return $ (,) path $ "REM " ++ show (CB.prDay prem) ++ " MSG " ++ CB.prTitle prem ++ "\n"

backend :: CB.Backend ItemID St
backend = CB.static_backend parseConfig load id reminderTemplate

