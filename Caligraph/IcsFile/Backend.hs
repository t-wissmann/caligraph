{-# LANGUAGE TemplateHaskell #-}
module Caligraph.IcsFile.Backend where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Caligraph.Utils as CU
import qualified Caligraph.PointerStore as PS

import Data.Either
import Data.Maybe
import Data.Foldable

import Data.Time.Calendar
import qualified Data.Array as A
import qualified Data.ByteString.Lazy as BS
import Data.Default

import System.FilePath
import Control.Monad
import Control.Monad.Writer
import Text.ParserCombinators.Parsec

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import qualified Caligraph.IcsFile.Types as Ics
import qualified Caligraph.IcsFile.Parser as IcsParser

data St = St
    { _path :: String -- a filepath with tilde not yet expanded
    , _idStore :: PS.PointerStore Int
    -- ^ store line numbers of calendar entries
    , _calendar :: Maybe (Ics.Tree SourcePos)
    -- , _events :: [Ics.CompiledEvent]
    , _bootup :: Bool
    -- ^ whether we are in the startup phase
    }

makeLenses ''St

data Event =
  CalendarLoaded (Either String (Ics.Tree SourcePos))
  | SourceEdited
  | FileChanged Bool
  -- ^ when the file was modified, and whether it still exists

parseConfig :: CB.ConfigRead -> Either String (St, CB.WakeUpLoop Event)
parseConfig cfg = do
  path <- mandatory CB.configFilePath "path"
  let st = St path PS.empty Nothing True
  return (st, wakeUpLoop st)
  where
    mandatory = CB.mandatory cfg

wakeUpLoop :: St -> CB.WakeUpLoop Event
wakeUpLoop st reportEvent = do
    CU.watchFile (st^.path) $ \exists ->
      reportEvent $ FileChanged exists

cachedIncarnations :: St -> (Day,Day) -> CB.Incarnations'
cachedIncarnations st (from,to) =
  A.accumArray (flip (:)) [] (from,to)
  $ filter (\ (d,_) -> from <= d && d <= to)
  $ [ (CB.day r, r) | r <- [] ]

handleEvent :: CB.Event Event -> CB.BackendM St Event ()
handleEvent (CB.SetRangeVisible (from,to)) = return ()
handleEvent (CB.AddReminder pr) = do
  tildepath <- use path
  return ()
  -- CB.callback ("Adding reminder to " ++ tildepath) $ do
  --   return ()

handleEvent (CB.Response (CalendarLoaded cOrError)) = do
  case cOrError of
    Right icsTree -> do
      -- c' <- zoom idStore $ mapM PS.lookupOrInsert c
      calendar .= Just icsTree
      tell [CB.BALog $ "Calendar loaded with "
            ++ (show $ length $ Ics.treeChildren icsTree) ++ " events"]
      -- forM_ (warnings :: [String]) (\w -> tell [CB.BAError w])
      return ()
    Left error -> tell [CB.BAError error]
handleEvent (CB.Response (SourceEdited)) = return ()
handleEvent (CB.Response (FileChanged exists)) = do
      fp <- use path
      if exists
      then tell [CB.BALog $ "File " ++ fp ++ " modified"] >> reloadFile
      else tell [CB.BALog $ "File " ++ fp ++ " removed"]

reloadFile :: CB.BackendM St Event ()
reloadFile = do
  fp <- use path
  CB.callback ("Loading " ++ fp) $ fmap CalendarLoaded $ do
    contentOrError <- IcsParser.parseFile fp
    return $ mapLeft show contentOrError
  where mapLeft f = either (Left . f) Right

backend :: CB.Backend St Event
backend = CB.Backend
  { CB.create = parseConfig
  , CB.cachedIncarnations = cachedIncarnations
  , CB.itemSource = (\ptr -> do
      -- maybe_path <- use calendar
      -- line <- zoom idStore $ PS.resolve ptr
      return CB.NoSource)
  , CB.handleEvent = (\ev -> do
      handleEvent ev
      inBootup <- use bootup
      when inBootup $ do
        bootup .= False
        reloadFile
      )
  }

