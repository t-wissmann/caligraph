
module Utils where

import Caligraph.Utils
import Tester

testUtils :: TestM ()
testUtils = do
    (10,0) `diffTime` (2,0) =!= (8,0)
    (10,30) `diffTime` (2,40) =!= (7,50)
    (2,30) `diffTime` (4,20) =!= (-1,-50)
    (2,20) `diffTime` (4,30) =!= (-2,-10)
    (0,0) `diffTime` (0,0) =!= (0,00)
    (23,59) `diffTime` (0,0) =!= (23,59)
