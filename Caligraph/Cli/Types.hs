module Caligraph.Cli.Types where

import Caligraph.Config.Types

import Brick
import Data.Time.Calendar

data Dir = DirUp | DirDown | DirLeft  | DirRight deriving (Eq,Show)

instance FinitelyManyNames Dir where
  finitelyManyNames =
    [ (,) "up" DirUp
    , (,) "down" DirDown
    , (,) "left" DirLeft
    , (,) "right" DirRight
    ]

instance UserReadShow Dir where
  userShow = showName
  userParser = parseName

instance Read Dir where
    readsPrec _ str = (\x -> [(x,"")]) $
        if str == "up" then DirUp else
        if str == "down" then DirDown else
        if str == "left" then DirLeft else
        if str == "right" then DirRight else
        error "a direction is up, down, left, or right"

data CliMode = CMNormal | CMInsert

data WidgetName =
    WNDayGrid
    | WNDay Day
    | WNDayItem Day Int
    | WNNewReminder
    deriving (Ord,Eq,Show)

-- | the type of a day widget
type DayWidget n
    = Int
    -- ^ the available width on the screen
    -> (Int,Widget n)
    -- ^ the widget and its height

type LogLine = String

emptyDay :: DayWidget n
emptyDay _ = (1, str "loading...")

-- | visual style of a calendar item / incarnation
data CalItemStyle = CalItemStyle
  { cisAttrName :: AttrName
  }

