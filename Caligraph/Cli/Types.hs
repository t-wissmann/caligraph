module Caligraph.Cli.Types where

import Brick
import Data.Time.Calendar

data Dir = DirUp | DirDown | DirLeft  | DirRight deriving (Eq,Show)

data CliMode = CMNormal | CMInsert

data WidgetName =
    WNDayGrid
    | WNDay Day
    | WNDayItem Day Int
    deriving (Ord,Eq,Show)

-- | the type of a day widget
type DayWidget n
    = Int
    -- ^ the available width on the screen
    -> (Int,Widget n)
    -- ^ the widget and its height

emptyDay :: DayWidget n
emptyDay _ = (1, str "loading...")

