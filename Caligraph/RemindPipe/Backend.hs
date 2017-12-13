{-# LANGUAGE TemplateHaskell #-}
module Caligraph.RemindPipe.Backend where

import Caligraph.RemindPipe.Types
import qualified Caligraph.Backend.Types as CB

import Control.Monad.State

import Data.Time.Calendar
import qualified Data.Array as A
import qualified Data.Map.Strict as M

import System.FilePath

import Lens.Micro.Mtl
import Lens.Micro.TH

-- basically parse the output of  remind -r -s -l file month year

type Identifier = (FilePath, Int)
data St = St
    { _path :: String
    , _monthCache :: M.Map (Integer,Int) (CB.Incarnations Identifier)
    }

makeLenses ''St

query :: (Day,Day) -> State St (CB.Incarnations Identifier)
query (from,to) = do
    (_, at_from, later) <- fmap (M.splitLookup (from_y,from_m)) $ use monthCache
    let (in_between, at_to, _) = M.splitLookup (to_y,to_m) later
    return $
        A.array (from,to)
        $ maybe [] A.assocs at_from
          ++ concatMap A.assocs (M.elems in_between)
          ++ maybe [] A.assocs at_to
    where
        (from_y,from_m,_) = toGregorian from
        (to_y,to_m,_) = toGregorian to

parseConfig :: (String -> Maybe String) -> Either String St
parseConfig cfg =
    case (cfg "path") of
        Just path -> return $ St path M.empty
        Nothing -> Left "Mandatory setting 'path' missing"


backend :: CB.Backend Identifier St
backend = CB.Backend
  { CB.query = query
  , CB.dequeueIO = const Nothing
  , CB.editExternally = (\i -> return ())
  , CB.addReminder = (\prem -> return ())
  , CB.create = parseConfig
  }


