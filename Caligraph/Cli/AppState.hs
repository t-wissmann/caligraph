{-# LANGUAGE TemplateHaskell #-}

module Caligraph.Cli.AppState where

import Caligraph.Backend.Types as CB
import Caligraph.Cli.Types
import Caligraph.Breakpoint
import qualified Caligraph.Calendar as Calendar
import qualified Caligraph.Cli.DayGrid as DayGrid
import qualified Brick.Widgets.Edit as Brick
import qualified Brick.BChan as Brick
import Graphics.Vty.Input.Events (Modifier,Key)

import Data.Array
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (Day)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Control.Monad.State
import System.Exit (ExitCode)

data AppMode
    = AMNormal
    | AMAppend
    deriving (Eq,Show)

data ExternalEvent
    = CalendarIO Int
    -- ^ a calendar io for the i'th calendar
    | ProcessFinished String ExitCode
    | ProcessOutput String String
    | ProcessError String String
    -- ^ one of the subprocesses finished

type Cmd st = StateT st (Breakpoint IO) ()

data AppState = AppState
    { _aboutToQuit :: Bool
    , _dayGrid :: DayGrid.St WidgetName
    , _dayRange :: (Day,Day)
    , _focusItem :: Maybe Int -- the item focused within a day, Nothing means 'the last'
    , _calendars :: [(Text,Calendar.Calendar)]
    , _messages :: [(UTCTime,LogLine)]
    , _mode :: AppMode
    , _newReminderEditor :: Brick.Editor String WidgetName
    , _eventChannel :: Brick.BChan ExternalEvent
    , _timeZone :: TimeZone
    , _showLogLines :: Int -- number of log lines to show
    , _binds :: Map.Map ([Modifier],Key) (Cmd AppState)
    }

makeLenses ''AppState

-- | a lens for accessing a calendar by its index
calendar_idx :: Functor f => Int -> (Calendar.Calendar -> f Calendar.Calendar) -> AppState -> f AppState
calendar_idx idx f st =
    fmap (\newCalendar ->
        st { _calendars =
                map (changer newCalendar) (zip [0..] $ _calendars st) })
        $ f $ snd $ (_calendars st) !! idx
    where
        changer :: (Eq n, Num n) => a -> (n,(b,a)) -> (b,a)
        changer newCal (i,(b,a)) =
            if i == fromIntegral idx
            then (b,newCal)
            else (b,a)
