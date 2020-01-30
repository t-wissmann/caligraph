
module Main where

import Tester
import System.Exit

import UnicodeJunctionSpec
import LazyResultSpec
import Utils
import BackendUtils
import BackendRemind
import BackendIcsFile
import Config

main :: IO ()
main = do
  res <- runTester specs
  if res then exitSuccess else exitFailure
  where
    specs = do
        someLookups
        testLazyResult
        testUtils
        testBackendUtils
        testBackendRemind
        testBackendIcsFile
        testConfig
