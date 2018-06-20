{-# LANGUAGE ExistentialQuantification #-}

module Caligraph.Backend.Registered where

import Caligraph.Backend.Types (XBackend)

import qualified Caligraph.Remind.Backend
import qualified Caligraph.RemindPipe.Backend

import Data.Hashable

data SomeBackend = forall stateType queryType.
        SomeBackend (XBackend  stateType queryType)

backends :: [(String, SomeBackend)]
backends =
    -- [ b "remindSimple"  Caligraph.Remind.Backend.backend
    --, b "remindPipe"    Caligraph.RemindPipe.Backend.backend
    []
    where b x y = (x, SomeBackend y)

