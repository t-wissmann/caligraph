
module Caligraph.Remind.Backend where

import Caligraph.Remind.Parser
import Caligraph.Remind.Types
import Data.Either
import Data.Maybe
import Control.Monad.Identity
import qualified Caligraph.Backend as CB
import Data.Time.Calendar (Day,addDays,diffDays,fromGregorian,gregorianMonthLength)

algorithm :: REM -> CB.Item
algorithm (REM args msg) =
   CB.Item
  { CB.lifetime =
        repetitionLifeTime args startDatePartial
  , CB.incarnations = incs
  }
  where
    startDatePartial =
      flatPartialDate $ findRemArg DateSpec args
    incs f t =
        case isFullDate startDatePartial of
          Nothing -> []
          Just startDate ->
             if t < startDate
             then [] -- If the reminder is in the future
             else
               case (findRemArg Repeat args) of
                  Just r ->
                    [ CB.Incarnation
                        d
                        (findRemArg AT args)
                        (findRemArg DURATION args)
                        msg
                    | d <- repetitionDays args startDate r f t
                    ]
                  Nothing ->
                    -- no repetition
                    [ CB.Incarnation
                        startDate
                        (findRemArg AT args)
                        (findRemArg DURATION args)
                        msg
                    ]

-- | on which days within an interval does a given reminder occur?
repetitionDays
  :: [RemArg]
  -- ^ the reminder
  -> Day
  -- ^ the DateSpecFull of the reminder
  -> Int
  -- ^ the Repeat of the reminder
  -> Day
  -- ^ the first day of the interval of interest
  -> Day
  -- ^ the last day of the interval of interest
  -> [Day]
  -- ^ the days within the interval on which the reminder occurs
repetitionDays args firstDay repeat f t =
    map (flip addDays firstDay) $ map ((*) $ toInteger repeat) [rep_from..rep_to]
        where
           day2repetitionIdx :: Fractional a => Day -> a
           day2repetitionIdx d = (fromIntegral $ d `diffDays` firstDay)
                               / (fromIntegral repeat)
           -- the index of the first repetition within in the interval
           rep_from = max 0 (ceiling $ day2repetitionIdx f)
           rep_end_day = min t $ fromMaybe t $ findRemArg UNTIL args
           -- the index of the last repetition within in the interval
           rep_to = max 0 (floor $ day2repetitionIdx rep_end_day)

repetitionLifeTime :: [RemArg] -> PartialDate -> (Maybe Day,Maybe Day)
repetitionLifeTime args pdate =
  case isFullDate pdate of
    Nothing -> partialDateLifeTime pdate
    Just startDate ->
      (,) (Just startDate) $
      case findRemArg Repeat args of
        Just r -> findRemArg UNTIL args
        Nothing -> (Just startDate)

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

init :: String -> IO CB.Backend
init path = do
  reminders <- parseFile path
  return $ CB.Backend $ extract_rems $ reminders
  where
    extract_rems :: [Either e (i,RFLine)] -> [CB.Item]
    extract_rems = map algorithm . mapMaybe isRem . map snd . rights
    isRem :: RFLine -> Maybe REM
    isRem (Rem r) = Just r
    isRem _ = Nothing



