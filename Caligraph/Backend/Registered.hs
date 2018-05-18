{-# LANGUAGE ExistentialQuantification #-}

module Caligraph.Backend.Registered where

import Caligraph.Backend.Types (Backend)

import qualified Caligraph.Remind.Backend
import qualified Caligraph.RemindPipe.Backend

import Data.Hashable

data SomeBackend = forall identType backendState.
    (Eq identType, Hashable identType) =>
        SomeBackend (Backend identType backendState)

backends :: [(String, SomeBackend)]
backends =
    [ b "remindSimple"  Caligraph.Remind.Backend.backend
    , b "remindPipe"    Caligraph.RemindPipe.Backend.backend
    ]
    where b x y = (x, SomeBackend y)

