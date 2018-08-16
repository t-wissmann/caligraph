
module Utils where

import Caligraph.Utils
import Tester
import Control.Monad.State

testUtils :: TestM ()
testUtils = do
    (10,0) `diffTime` (2,0) =!= (8,0)
    (10,30) `diffTime` (2,40) =!= (7,50)
    (2,30) `diffTime` (4,20) =!= (-1,-50)
    (2,20) `diffTime` (4,30) =!= (-2,-10)
    (0,0) `diffTime` (0,0) =!= (0,00)
    (23,59) `diffTime` (0,0) =!= (23,59)
    testForState

plus1 :: State Int Int
plus1 = do
    n <- get
    put (n+1)
    return n

testForState :: TestM ()
testForState =
    let
        input = [1..4]
        c1 :: State [Int] [Int]
        c1 = forState (plus1)
        c2 :: State [Int] [Int]
        c2 = do modify (map ((+) 1)) ; return input
    in
    runState c1 input =!= runState c2 input
