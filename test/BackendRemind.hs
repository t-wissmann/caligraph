module BackendRemind where

import Tester
import Caligraph.Remind.Backend
import Caligraph.RemindPipe.Backend
import Caligraph.Backend.Types
import Data.Time.Calendar
import Control.Monad
import Data.Ix
import Data.List

testBackendRemind =
    let
        firstjan = fromGregorian 2016 1 1
        lastdec = fromGregorian 2017 12 31
    in do
    templateTest
    monthsCoveredSpec (firstjan, lastdec)
    forM_ (range (firstjan, lastdec))
        (\date -> monthsCoveredSpec (date,date))

monthsCoveredSpec :: (Day,Day) -> TestM ()
monthsCoveredSpec dayRange =
    monthsCovered dayRange =!=
        (map head
            $ group
            $ map (\(y,m,_) -> (y,m))
            $ map toGregorian $ range dayRange)

templateTest =
    (reminderTemplate (PartialReminder
        (fromGregorian 2018 1 2)
        "test"
        (Just (8,0))
        (Just (3,4))
        (Just (fromGregorian 2018 11 12, 9)))
    =!=
    "REM 2018-01-02 AT 8:00 DURATION 3:04 *9 UNTIL 2018-11-12 MSG [\"test\"]\n")


