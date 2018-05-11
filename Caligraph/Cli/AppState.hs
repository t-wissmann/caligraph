{-# LANGUAGE TemplateHaskell #-}

module Caligraph.Cli.AppState where

import Caligraph.Backend.Types as CB
import Caligraph.Cli.Types
import qualified Caligraph.Calendar as Calendar
import qualified Caligraph.Cli.DayGrid as DayGrid

import Data.Array
import Data.Time.Calendar (Day)

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

data AppState = AppState
    { _aboutToQuit :: Bool
    , _dayGrid :: DayGrid.St WidgetName
    , _visibleIncarnations :: Array Day [CB.Incarnation']
    , _focusItem :: Maybe Int -- the item focused within a day, Nothing means 'the last'
    , _calendar :: Calendar.Calendar
    , _messages :: [LogLine]
    }

makeLenses ''AppState
