
module Main where

import Tester
import UnicodeJunctionSpec
import LazyResultSpec

main :: IO Int
main =
  runTester $ do
    someLookups
    testLazyResult
