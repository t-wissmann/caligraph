
module Caligraph.Remind.Backend where

import Caligraph.Remind.Parser
import Caligraph.Remind.Types
import Data.Either
import Data.Maybe
import qualified Caligraph.Backend as CB

algorithm :: REM -> CB.Item
algorithm (REM args msg) = CB.Item
  { CB.lifetime = (Nothing,Nothing)
  , CB.incarnations = \f t -> []
  }

init :: String -> IO CB.Backend
init path = do
  reminders <- parseFile path
  return $ CB.Backend $ extract_rems $ reminders
  where
    extract_rems :: [Either e (i,RFLine)] -> [CB.Item]
    extract_rems = map algorithm . mapMaybe isRem . map snd . rights
    isRem :: RFLine -> Maybe REM
    isRem (Rem r) = Just r
    isRem _ = Nothing



