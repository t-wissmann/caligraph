
module Caligraph.Remind.Backend where

import Caligraph.Remind.Parser
import Caligraph.Remind.Types
import Data.Either
import Data.Maybe
import Control.Monad.Identity
import qualified Caligraph.Backend as CB
import Data.Time.Calendar (Day,fromGregorian,gregorianMonthLength)

algorithm :: REM -> CB.Item
algorithm (REM args msg) =
   CB.Item
  { CB.lifetime =
        partialDateLifeTime start_date
  , CB.incarnations = incs
  }
  where
    start_date = flatPartialDate $ findRemArg Date_spec args
    incs f t =
        case isFullDate start_date of
            Just d ->
                if f <= d && d <= t then
                [ CB.Incarnation
                    d
                    (findRemArg AT args)
                    (findRemArg DURATION args)
                    msg
                ]
                else
                []
            Nothing -> []

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



