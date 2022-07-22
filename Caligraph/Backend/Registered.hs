{-# LANGUAGE ExistentialQuantification #-}

module Caligraph.Backend.Registered where

import Caligraph.Backend.Types (Backend)

import qualified Caligraph.Remind.Backend
import qualified Caligraph.IcsFile.Backend
import qualified Caligraph.RemindPipe.Backend
import qualified Caligraph.Plaintext.Backend
import qualified Caligraph.ICalendar.Backend

import Data.Hashable

data SomeBackend = forall stateType eventType.
        SomeBackend (Backend stateType eventType)

backends :: [(String, SomeBackend)]
backends =
    [ b "remindSimple"  Caligraph.Remind.Backend.backend
    , b "remindPipe"    Caligraph.RemindPipe.Backend.backend
    , b "plaintext"     Caligraph.Plaintext.Backend.backend
    , b "icsfile-custom" Caligraph.IcsFile.Backend.backend
    , b "icsfile-icalendar" Caligraph.ICalendar.Backend.backend
    ]
    where b x y = (x, SomeBackend y)

-- | pairs of regexes for the 'path' setting toegher with the name of a backend
-- that can handle this kind of 'path'. The regexes should be applied case
-- insensitive.
pathRegex2backend :: [(String,String)]
pathRegex2backend =
  [ (,) ".*\\.rem$" "remindPipe"
  , (,) ".*\\.txt$" "plaintext"
  , (,) ".*\\.ics$" "icsfile-icalendar"
  ]

