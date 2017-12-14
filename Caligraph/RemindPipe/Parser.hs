module Caligraph.RemindPipe.Parser where

import Caligraph.RemindPipe.Types
import qualified Caligraph.Backend.Types as CB

import Data.Time.Calendar

parseRemOutput :: String -> [(Day,CB.Incarnation Identifier)]
parseRemOutput = const []
