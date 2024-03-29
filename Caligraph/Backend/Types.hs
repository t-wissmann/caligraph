module Caligraph.Backend.Types where

import Data.Time.Calendar (Day)
import Data.Time.Calendar (Day,addDays,diffDays)
import Caligraph.PointerStore (Ptr)
import Caligraph.Cli.Types (LogLine)

import Text.Printf
import Data.Ord
import Data.Array
import qualified Data.List as List
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

data Incarnation i = Incarnation
  { day :: Day
  , time :: Maybe (Int,Int)
  , duration :: Maybe (Int,Int)
  , title :: String
  -- , TODO: a timezone
  , itemId :: i
  } deriving (Eq,Show)

instance Functor Incarnation where
    fmap f x = x { itemId = f $ itemId x }

instance Foldable Incarnation where
    foldMap f t = f $ itemId t

instance Traversable Incarnation where
    traverse f inc = pure (\x -> inc { itemId = x }) <*> (f $ itemId inc)

type Incarnation' = Incarnation Ptr

instance Ord i => Ord (Incarnation i) where
  compare i1 i2 =
    either id (\() -> EQ) $ do
      -- sorting is basically lexicographic
      compare_on day i1 i2
      -- but we want Just x to be smaller than Nothing
      compare_on (Down . fmap Down . time) i1 i2
      compare_on duration i1 i2
      compare_on title i1 i2
      compare_on itemId i1 i2
    where compare_on projection v1 v2
            =
            case compare (projection v1) (projection v2) of
            EQ -> Right ()
            LT -> Left LT
            GT -> Left GT

data PartialReminder = PartialReminder
    { prDay :: Day
    , prTitle :: String
    , prTime :: Maybe (Int,Int)
    , prDuration :: Maybe (Int,Int)
    , prUntil :: Maybe (Day,Int)
    -- ^ if the reminder spans multiple days: until when
    --   and after how many days does it repeat
    }

data Item i = Item
  { lifetime :: (Maybe Day, Maybe Day)
  -- ^ the interval in which incarnations of this reminder live
  , incarnations :: Day -> Day -> [Incarnation i]
  -- ^ all incarnations within a (closed) interval
  , identifier :: i
  }

type Incarnations i = Array Day [Incarnation i]
type Incarnations' = Array Day [Incarnation']

data BackendQuery a = BackendQuery
    { bqMesg :: String
    , bqIO :: (IO a)
    }

data BackendAction a =
      BAQuery (BackendQuery a)
    | BAError LogLine
    | BALog LogLine

type BackendM state event a = StateT state (Writer [BackendAction event]) a

-- | tell where the source code of an item can be edited
data ItemSource event =
      ExistingFile (FilePath, Int) event
      -- ^ in a specific line of an existing file, together with an event
      --   that is triggered when the user finished editing
      -- | Source Text (Text -> queryType)
      -- ^ the source is provided, together with an event that updates the item
      | NoSource

data Event event =
      SetRangeVisible (Day,Day)
    | AddReminder PartialReminder
    | Response event

type ConfigGetter a = String -> Maybe a

data ConfigRead = ConfigRead
  { configString :: ConfigGetter String
  , configFilePath :: String -> Maybe FilePath
  -- ^ returns a filepath. if the config starts with './', then the path is made absolute
  }

type WakeUpLoop event = (event -> IO ()) -> IO ()

data Backend state event = Backend
  { create :: ConfigRead -> Either String (state, WakeUpLoop event)
  , cachedIncarnations :: state -> (Day,Day) -> Incarnations'
  , handleEvent :: Event event -> BackendM state event ()
  , itemSource :: Ptr -> BackendM state event (ItemSource event)
  }

