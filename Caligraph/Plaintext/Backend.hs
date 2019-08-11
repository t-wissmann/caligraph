{-# LANGUAGE TemplateHaskell #-}
module Caligraph.Plaintext.Backend where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Caligraph.Utils as CU
import qualified Caligraph.PointerStore as PS

import Data.Either
import Data.Foldable

import Data.Time.Calendar
import qualified Data.Array as A

import System.FilePath
import Control.Monad
import Control.Monad.Writer
import Text.ParserCombinators.Parsec

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data Calendar i = Calendar
  { calFilePath :: FilePath
  , calReminders :: [CB.Incarnation i]
  -- ^ we just have single entries
  }

instance Functor Calendar where
    fmap f x = x { calReminders = map (fmap f) $ calReminders x }

instance Foldable Calendar where
    foldMap a2m cal = fold $ map CB.itemId $ calReminders $ fmap a2m cal

instance Traversable Calendar where
    traverse f cal =
      pure (\x -> cal { calReminders = x })
      <*> (traverse (traverse f) (calReminders cal))

data St = St
    { _path :: String -- a filepath with tilde not yet expanded
    , _idStore :: PS.PointerStore Int
    -- ^ store line numbers of calendar entries
    , _calendar :: Maybe (Calendar PS.Ptr)
    , _bootup :: Bool
    -- ^ whether we are in the startup phase
    }

makeLenses ''St

data Event =
  CalendarLoaded (Either String (Calendar Int))
  | SourceEdited

parseConfig :: (String -> Maybe String) -> Either String (St, CB.WakeUpLoop Event)
parseConfig cfg =
  pure (,)
    <*> (pure (\p -> St p PS.empty Nothing True) <*> mandatory "path")
    <*> pure (\cb -> return ())
  where
    mandatory :: String -> Either String String
    mandatory key =
      maybe (Left $ "Mandatory key " ++ key ++ " missing") Right (cfg key)

cachedIncarnations :: St -> (Day,Day) -> CB.Incarnations'
cachedIncarnations st (from,to) =
  A.array (from,to) [(d, f d) | d <- [from..to]]
  where
    f d =
      filter (\r -> d == CB.day r)
      $ maybe [] id
      $ fmap calReminders
      $ _calendar st

handleEvent :: CB.Event Event -> CB.BackendM St Event ()
handleEvent (CB.SetRangeVisible (from,to)) = return ()
handleEvent (CB.AddReminder pr) = return ()
handleEvent (CB.Response (CalendarLoaded cOrError)) = do
  case cOrError of
    Right c -> do
      c' <- zoom idStore $ mapM PS.lookupOrInsert c
      calendar .= Just c'
    Left error -> tell [CB.BAError error]
handleEvent (CB.Response (SourceEdited)) = return ()

calendarParser :: GenParser Char st [CB.Incarnation Int]
calendarParser = item `endBy` (char '\n')
  where
    item = do
      day <- read <$> (many1 $ oneOf "0123456789-")
      char ' '
      l <- fmap sourceLine getPosition
      reminder_src <- many (noneOf "\n")
      let desc = reminder_src
      return $ CB.Incarnation day Nothing Nothing desc l

reloadFile :: CB.BackendM St Event ()
reloadFile = do
  fp <- use path
  CB.callback ("Loading " ++ fp) $ fmap CalendarLoaded $ do
    fullpath <- CU.expandTilde fp
    input <- readFile fullpath
    return $ mapLeft show
           $ parse (pure (Calendar fullpath) <*> calendarParser) fp input
  where mapLeft f = either (Left . f) Right

backend :: CB.Backend St Event
backend = CB.Backend
  { CB.create = parseConfig
  , CB.cachedIncarnations = cachedIncarnations
  , CB.itemSource = (\ptr -> do
      return $ CB.NoSource
      )
  , CB.handleEvent = (\ev -> do
      handleEvent ev
      inBootup <- use bootup
      when inBootup $ do
        bootup .= False
        reloadFile
      )
  }

