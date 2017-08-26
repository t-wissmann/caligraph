{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Caligraph.Cli.DayGrid where

import qualified Caligraph.Cli.UnicodeJunction as UnicodeJunction

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr, cropTopBy, cropBottomBy,setAvailableSize)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style

import Data.Time.Calendar (Day,addDays,diffDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)

import qualified Data.List as L

import qualified System.Console.Terminal.Size as TerminalSize

import Lens.Micro
import Lens.Micro.TH

data DayWidget n = DayWidget
  { widget :: Day -> Widget n
  , height :: Day -> Int
  }

-- | The internal state
data St = St
  { _scrollOffset :: Int -- the number of rows hidden of _scrollDay
  , _scrollDay :: Day
  , _focusDay :: Day
  , _today :: Day
  , _size :: (Int,Int)
  , _rows :: [([Day],Int,Int)] -- the visible rows
  -- , _columns ::Int -- the number of days per row
  }

makeLenses ''St

init
  :: (Int,Int)
  -- ^ the widget size
  -> Day
  -- ^ today
  -> St
init size d =
  computeVisibleRows $ St 0 d d d size []

getToday :: IO Day
      -- ^ today
getToday = do
  g <- getCurrentTime
  return $ utctDay g

getScreenSize :: IO (Int,Int)
           -- ^ the width and height of the screen
getScreenSize = do
  termSize <- TerminalSize.size
  return $ case termSize of
    Nothing -> (40,80)
    Just (TerminalSize.Window h w) -> (w,h)

-- | render the calendar
render :: (Ord n, Show n)
  => (Day -> Widget n)
  -- ^ Rendering functor for a day
  -> St
  -- ^- Internal State
  -> Widget n
  -- ^ Rendered widget
render day2widget s =
  (s^.rows)
  & map (\(days,height,cb) ->
        map (renderDay s day2widget) days
        & map (setAvailableSize (daywidth,height))
        & map (cropBottomBy cb)
        & hBox
        )
  & mapHead (cropTopBy (s^.scrollOffset))
  & vBox
  where (fullwidth,_) = s^.size
        daywidth = (fullwidth - 1) `div` 7
        mapHead _ [] = []
        mapHead f (x:xs) = (f x:xs)

data Surrounding celltype = Surrounding
  { nNeighbour :: Maybe celltype -- North
  , nwNeighour :: Maybe celltype -- North west neighbour
  , wNeighbour :: Maybe celltype -- west neighbour
  } deriving (Functor)

surroundingDays :: St -> Day -> Surrounding Day
surroundingDays _ day =
  Surrounding n nw w
  where
    (_,_,dayOfWeek) = toWeekDate day
    n = Just $ addDays (-7) day
    [nw,w] =
      case dayOfWeek of
        1 -> [Nothing,Nothing]
        _ -> map Just $ map (\x -> addDays x day) [-8,-1]

renderDay :: St -> (Day -> Widget n) -> Day -> Widget n
renderDay st day2widget day =
  ((topleftJunction <+> UnicodeJunction.withLineType topBorder hBorder)
    <=> (UnicodeJunction.withLineType leftBorder vBorder
         <+> ((if day == blackBgDay then withAttr "focusDay" else id) $
                        day2widget day
        <=> fill ' ')))
  where
    boldDay = st^.focusDay
    blackBgDay = st^.today
    thisDayBold = Just $ day == boldDay
    (Surrounding north northwest west) = fmap ((==) boldDay) $ surroundingDays st day
    maybeOr (Just True) _  = Just True
    maybeOr _ (Just True)  = Just True
    maybeOr (Just False) _ = Just False
    maybeOr _ (Just False) = Just False
    maybeOr _ _            = Nothing
    topBorder =
      UnicodeJunction.fromMaybeBold $ thisDayBold `maybeOr` north
    leftBorderOfNorth = -- the style of the left border of the northern neighbour
      UnicodeJunction.fromMaybeBold $ north `maybeOr` northwest
    topBorderOfWest = -- the style of the top border of the western neighbour
      UnicodeJunction.fromMaybeBold $ west `maybeOr` northwest
    leftBorder =
      UnicodeJunction.fromMaybeBold $ thisDayBold `maybeOr` west
    topleftJunction =
      str [UnicodeJunction.get
              leftBorderOfNorth topBorder
              leftBorder topBorderOfWest]

scrollToFocus :: St -> St
scrollToFocus st =
  case L.find (\(_,(ds,_,_)) -> st^.focusDay `elem` ds) (zip [0..] $ st^.rows) of
    -- if the focus day is among the rows, but in the first row
    Just (0,(_,_,0)) ->
      -- focusDay is only fully visible if we do not have any scroll ofset
      if (st^.scrollOffset > 0)
      then st & scrollOffset .~ 0 & computeVisibleRows
      else st
    -- if the focus day is in the first row but this row is cropped at the bottom
    -- then the screen is too small and we can't do anything
    Just (0,(_,_,_)) ->
      st
    -- if the focus day is not in the first row (i.e. not cropped at the top)
    -- and not cropped from the bottom, then it's already fully visible
    Just (_,(_,_,0)) ->
      st
    -- if it is cropped at the bottom, then scroll to the top accordingly
    Just (_,(_,_,cb)) ->
      st & scrollOffset %~ (\o -> o + cb) & computeVisibleRows
    -- if the focused day is not among the rows at all, then find out whether we
    -- need to go to the future or past
    Nothing ->
      if ((st^.focusDay) `diffDays` (st^.scrollDay) > 0)
      -- if focused day is in the future
      then st & scrollDay .~ (st^.focusDay)
              & scrollOffset .~ (daysHeight st (weekOf (st^.focusDay)) - (snd $ st^.size))
              & computeVisibleRows
      -- if focused day is in the past
      else st & scrollDay .~ st^.focusDay
              & scrollOffset .~ 0
              & computeVisibleRows

computeVisibleRows :: St -> St
computeVisibleRows st =
  if (st^.scrollOffset < 0)
  then st & scrollDay %~ (addDays (-7))
          & (\s -> s & scrollOffset %~ (\x -> x + weekHeight s))
          & computeVisibleRows
  else
    if (st^.scrollOffset >= weekHeight st)
    then st & scrollDay %~ (addDays (7))
            & (\s -> s & scrollOffset %~ (\x -> x - weekHeight st))
            & computeVisibleRows
    else
    st & rows .~ new_rows_around (st^.scrollDay) (st^.scrollOffset)
  where
    weekHeight s = daysHeight s (weekOf $ s^.scrollDay)
    addWeeksLater :: Int -> [Day] -> [([Day],Int,Int)]
    addWeeksLater space_remain week =
      if space_remain <= curheight
      then [(week, curheight, curheight - space_remain)]
      else
        (week, curheight, 0) : (addWeeksLater (space_remain - curheight) $ map (addDays 7) week)
      where curheight = daysHeight st week

    new_rows_around day offset =
      weekOf day
      & addWeeksLater ((snd $ st^.size) + offset)


weekOf :: Day -> [Day]
weekOf day =
  [1 - toInteger dayOfWeek .. 7 - toInteger dayOfWeek]
  & map (flip addDays day)
  where (_,_,dayOfWeek) = toWeekDate day

daysHeight :: St -> [Day] -> Int
daysHeight s ds = foldr max 0 $ map (dayHeight s) ds

dayHeight :: St -> Day -> Int
dayHeight _ _ = 20



