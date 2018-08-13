
module BackendUtils where

import Caligraph.Backend.Utils
import Tester

testBackendUtils = do
    failsFor "8-asdfjlk ksdf"
    failsFor "-8 ksdf"
    p "8-9 sdf" =!= (Just (8,0), Just (1,0), "sdf")
    p "8 sdf" =!= (Just (8,0), Nothing, "sdf")
    t "8:00" =!= Just (8,0)
    t "14:00" =!= Just (14,0)
    t "8:03pm" =!= Just (20,3)
    t "9pm" =!= Just (21,0)
    t "1am" =!= Just (1,0)
    fails "8:00:"
    fails ":00"
    where
        p = parseTimeDuration
        t = tryParseTime
        failsFor str = p str =!= (Nothing,Nothing,str)
        fails str = t str =!= Nothing

