{-# LANGUAGE ExistentialQuantification #-}

module Caligraph.Backend.Registered where

import Caligraph.Backend.Types (Backend)

import qualified Caligraph.Remind.Backend
import qualified Caligraph.IcsFile.Backend
import qualified Caligraph.RemindPipe.Backend
import qualified Caligraph.Plaintext.Backend

import Data.Hashable

data SomeBackend = forall stateType eventType.
        SomeBackend (Backend stateType eventType)

backends :: [(String, SomeBackend)]
backends =
    [ b "remindSimple"  Caligraph.Remind.Backend.backend
    , b "remindPipe"    Caligraph.RemindPipe.Backend.backend
    , b "plaintext"     Caligraph.Plaintext.Backend.backend
    , b "icsfile"       Caligraph.IcsFile.Backend.backend
    ]
    where b x y = (x, SomeBackend y)

