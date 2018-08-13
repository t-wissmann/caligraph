{-# LANGUAGE TemplateHaskell #-}

module Caligraph.Cli.AppState where

import Caligraph.Backend.Types as CB
import Caligraph.Cli.Types
import qualified Caligraph.Calendar as Calendar
import qualified Caligraph.Cli.DayGrid as DayGrid
import qualified Brick.Widgets.Edit as Brick

import Data.Array
import Data.Time.Calendar (Day)
import Data.Text (Text)

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

data AppMode
    = AMNormal
    | AMAppend
    deriving (Eq,Show)

data AppState = AppState
    { _aboutToQuit :: Bool
    , _dayGrid :: DayGrid.St WidgetName
    , _visibleIncarnations :: Array Day [CB.Incarnation']
    , _focusItem :: Maybe Int -- the item focused within a day, Nothing means 'the last'
    , _calendars :: [(Text,Calendar.Calendar)]
    , _messages :: [LogLine]
    , _mode :: AppMode
    , _newReminderEditor :: Brick.Editor String WidgetName
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
