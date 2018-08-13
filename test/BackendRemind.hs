module BackendRemind where

import Tester
import Caligraph.Remind.Backend
import Caligraph.Backend.Types
import Data.Time.Calendar

testBackendRemind =
    reminderTemplate (PartialReminder
        (fromGregorian 2018 1 2)
        "test"
        (Just (8,0))
        (Just (3,4))
        (Just (fromGregorian 2018 11 12, 9)))
    =!=
    "REM 2018-01-02 AT 8:00 DURATION 3:04 *9 UNTIL 2018-11-12 MSG test\n"



