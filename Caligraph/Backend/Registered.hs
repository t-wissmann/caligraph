{-# LANGUAGE ExistentialQuantification #-}

module Caligraph.Backend.Registered where

import Caligraph.Backend.Types (Backend)

import qualified Caligraph.Remind.Backend as Remind

data SomeBackend = forall b. SomeBackend (Backend b)
backends :: [(String, SomeBackend)]
backends =
    [ b "remind" Remind.backend
    ]
    where b x y = (x, SomeBackend y)

