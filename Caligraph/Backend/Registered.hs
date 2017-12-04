{-# LANGUAGE ExistentialQuantification #-}

module Caligraph.Backend.Registered where

import Caligraph.Backend.Types (Backend)

import qualified Caligraph.Remind.Backend as Remind

import Data.Hashable

data SomeBackend = forall identType backendState.
    (Eq identType, Hashable identType) =>
        SomeBackend (Backend identType backendState)

backends :: [(String, SomeBackend)]
backends =
    [ b "remind" Remind.backend
    ]
    where b x y = (x, SomeBackend y)

