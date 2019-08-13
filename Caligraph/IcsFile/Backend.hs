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

import System.FilePath
import Control.Monad
import Control.Monad.Writer
import Text.ParserCombinators.Parsec

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data Calendar i = Calendar
  { calFilePath :: FilePath
  -- ^ full filepath without tilde
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
  | FileChanged Bool
  -- ^ when the file was modified, and whether it still exists

parseConfig :: (String -> Maybe String) -> Either String (St, CB.WakeUpLoop Event)
parseConfig cfg = do
  path <- mandatory "path"
  let st = St path PS.empty Nothing True
  return (st, wakeUpLoop st)
  where
    mandatory :: String -> Either String String
    mandatory key =
      maybe (Left $ "Mandatory key " ++ key ++ " missing") Right (cfg key)

wakeUpLoop :: St -> CB.WakeUpLoop Event
wakeUpLoop st reportEvent = do
    filepath <- liftIO $ CU.expandTilde (st^.path)
    CU.watchFile filepath $ \exists ->
      reportEvent $ FileChanged exists

cachedIncarnations :: St -> (Day,Day) -> CB.Incarnations'
cachedIncarnations st (from,to) =
  A.accumArray (flip (:)) [] (from,to)
  $ filter (\ (d,_) -> from <= d && d <= to)
  $ [ (CB.day r, r) | r <- maybe [] calReminders $ _calendar st ]

handleEvent :: CB.Event Event -> CB.BackendM St Event ()
handleEvent (CB.SetRangeVisible (from,to)) = return ()
handleEvent (CB.AddReminder pr) = do
  tildepath <- use path
  CB.callback ("Adding reminder to " ++ tildepath) $ do
    fullpath <- CU.expandTilde tildepath
    let endTime = pure timePlus <*> (CB.prTime pr) <*> (CB.prDuration pr)
    appendFile fullpath $ concat
      [ show $ CB.prDay pr
      , maybe "" ((++) " ") $ fmap showtime (CB.prTime pr)
      , maybe "" ((++) "-") $ fmap showtime endTime
      , " "
      , CB.prTitle pr
      , "\n"
      ]
    return SourceEdited
    where
      showtime (h,m) =
        show h ++ ":"
        ++ if m < 10 then "0" else "" ++ show m
      timePlus (fh,fm) (dh,dm) =
        (fh + dh + ((fm + dm) `div` 60), (fm + dm) `mod` 60)

handleEvent (CB.Response (CalendarLoaded cOrError)) = do
  case cOrError of
    Right c -> do
      c' <- zoom idStore $ mapM PS.lookupOrInsert c
      calendar .= Just c'
    Left error -> tell [CB.BAError error]
handleEvent (CB.Response (SourceEdited)) = return ()
handleEvent (CB.Response (FileChanged exists)) = do
      fp <- use path
      if exists
      then tell [CB.BALog $ "File " ++ fp ++ " modified"] >> reloadFile
      else tell [CB.BALog $ "File " ++ fp ++ " removed"]

calendarParser :: GenParser Char st [CB.Incarnation Int]
calendarParser = fmap catMaybes $
  (try comment <|> item) `endBy` (many1 $ char '\n')
  where
    comment = do
      many (char ' ')
      char '#'
      many (noneOf "\n")
      return Nothing
    item = fmap Just $ do
      day <- read <$> (many1 $ oneOf "0123456789-")
      char ' '
      l <- fmap sourceLine getPosition
      reminder_src <- many (noneOf "\n")
      let (from,duration,title) = CB.parseTimeDuration reminder_src
      return $ CB.Incarnation day from duration title l

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
      maybe_path <- use calendar
      line <- zoom idStore $ PS.resolve ptr
      return $ case fmap calFilePath maybe_path of
        Nothing -> CB.NoSource
        Just p -> CB.ExistingFile (p, line) SourceEdited)
  , CB.handleEvent = (\ev -> do
      handleEvent ev
      inBootup <- use bootup
      when inBootup $ do
        bootup .= False
        reloadFile
      )
  }

