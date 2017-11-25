
module Caligraph.Cli.Types where

import Brick
import Data.Time.Calendar

data Dir = DirUp | DirDown | DirLeft  | DirRight

data WidgetName =
    WNDayGrid
    | WNDay Day
    deriving (Ord,Eq,Show)

-- | the type of a day widget
type DayWidget n
    = Int
    -- ^ the available width on the screen
    -> (Int,Widget n)
    -- ^ the widget and its height

emptyDay :: DayWidget n
emptyDay _ = (1, str "loading...")

