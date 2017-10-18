{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Caligraph.Cli.DayGrid where

import qualified Caligraph.Cli.UnicodeJunction as UnicodeJunction

import Caligraph.Utils
import Data.Maybe
import Caligraph.LazyResult

import Brick
import Brick.Widgets.Border
import qualified Brick.Types
import Brick.Main
import Brick.Widgets.Core (withAttr, cropTopBy, cropBottomBy,setAvailableSize)
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style

import Data.Time.Calendar (Day,addDays,diffDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

import Control.Monad.Reader
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Extras (view)
import Data.Functor.Const

-- | the type of a day widget
type DayWidget n
    = Int
    -- ^ the available width on the screen
    -> (Int,Widget n)
    -- ^ the widget and its height

emptyDay :: DayWidget n
emptyDay _ = (1, str "loading...")

data Surrounding celltype = Surrounding
  { nwNeighour :: Maybe celltype -- north west neighbour
  , nNeighbour :: Maybe celltype -- north
  , neNeighbour :: Maybe celltype -- north east neighbour
  , wNeighbour :: Maybe celltype -- west neighbour
  } deriving (Functor)

-- | what to display within a row
data RowController = RowController
  { _rowOf :: Day -> [Day]
  , _columns :: Int
  , _previousRow :: [Day] -> [Day]
  , _nextRow :: [Day] -> [Day]
  , _surroundingDays :: Day -> Surrounding Day
  , _dayWidth :: Day -> Int -> Int -- ^ Given full width return a day's width
  }

makeLenses ''RowController

-- getter x f s = Const $ getConst $ f (x s)
-- columnsL = getter columns

weekPerRow :: RowController
weekPerRow = RowController
  { _rowOf = \day ->
      let (_,_,dayOfWeek) = toWeekDate day in
      [1 - toInteger dayOfWeek .. 7 - toInteger dayOfWeek]
      & map (flip addDays day)

  , _columns = 7
  , _surroundingDays = \day ->
      let (_,_,dayOfWeek) = toWeekDate day in
      fmap (flip addDays day) $
      case dayOfWeek of
        1 -> Surrounding Nothing (Just (-7)) (Just (-6)) Nothing
        7 -> Surrounding (Just (-8)) (Just (-7)) Nothing (Just (-1))
        _ -> Surrounding (Just (-8)) (Just (-7)) (Just (-6)) (Just (-1))
  , _previousRow = map (addDays (-7))
  , _nextRow     = map (addDays ( 7))
  , _dayWidth = const (`div` 7)
  }

-- | The internal state
data St n = St
  { _scrollOffset :: Int -- the number of rows hidden of _scrollDay
  , _scrollDay :: Day
  , _focusDay :: Day
  , _today :: Day
  , _size :: (Int,Int)
  , _day2widget :: Day -> (DayWidget n)
  , _dayCache :: Map Day (DayWidget n)
  , _rowController :: RowController
  -- , _columns ::Int -- the number of days per row
  }

makeLenses ''St

type StDays n = LazyResult Day (DayWidget n) (St n)


init
  :: (Int,Int)
  -- ^ the widget size
  -> Day
  -- ^ today
  -> St n
init size d =
  St 0 d d d size (const emptyDay) M.empty weekPerRow

getToday :: IO Day
      -- ^ today
getToday = do
  g <- getCurrentTime
  return $ utctDay g

-- | render the calendar
render :: (Ord n, Show n)
  => St n
  -- ^- Internal State
  -> Widget n
  -- ^ Rendered widget
render st =
  Widget Greedy Greedy render'
  where
    mapHead _ [] = []
    mapHead f (x:xs) = (f x:xs)
    render' = do
      fullwidth <- asks (view availWidthL)
      height <- asks (view availHeightL)
      daywidth <- return ((fullwidth - 1) `div` 7)
      Brick.Types.render $
        computeVisibleRows st (fullwidth,height)
        & map (\(days,height,cb) ->
          map (renderDay st daywidth) days
          & flip (++) [(1, renderRightmostBorder st $ last days)]
          & map snd
          & map (setAvailableSize (daywidth,height))
          & map (cropBottomBy cb)
          & hBox
        )
        & mapHead (cropTopBy (st^.scrollOffset))
        & vBox

computeVisibleRows :: St n -> (Int,Int) -> [([Day],Int,Int)]
computeVisibleRows st (fullwidth,fullheight) =
  remainingRows firstrow fullheight
  where
    rc :: RowController
    rc = st^.rowController
    firstrow = (rc^.rowOf) (st^.scrollDay)
    remainingRows :: [Day] -> Int -> [([Day],Int,Int)]
    remainingRows row rem_height =
      let row_height = daysHeight st fullwidth row in
      if rem_height <= row_height
      then [(row,row_height,row_height - rem_height)]
      else
        (row,row_height,0)
        : remainingRows
            (st^.rowController.nextRow $ row)
            (rem_height - row_height)

maybeOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
maybeOr (Just True) _  = Just True
maybeOr _ (Just True)  = Just True
maybeOr (Just False) _ = Just False
maybeOr _ (Just False) = Just False
maybeOr _ _            = Nothing

renderDay :: St n -> Int -> Day -> (Int, Widget n)
renderDay st width day =
  ( dayHeight + 1
  , (topleftJunction <+> topBorderWidget)
    <=> (leftBorderWidget
         <+> ((if day == blackBgDay then withAttr "focusDay" else id) $
                        dayWidget
        <=> fill ' ')))
  where
    (dayHeight,dayWidget) = (st^.day2widget) day (width-1)
    boldDay = st^.focusDay
    blackBgDay = st^.today
    surrounding_days =
      fmap ((==) boldDay) $ (st^.rowController.surroundingDays) day
    thisDayBold = Just $ day == boldDay
    (leftBorderWidget, topleftJunction, topBorderWidget) =
      renderBorder st thisDayBold surrounding_days

renderBorder
  :: St n
  -> Maybe Bool
  -- ^- wether this day is bold, or Nothing if 'this day' does not exist
  -> Surrounding Bool
  -- ^- whether the surrounding days are bold
  -> (Widget n, Widget n, Widget n)
  -- ^- the widgets for the left border, the topright junction, and the top border
renderBorder st thisDayBold (Surrounding northwest north _ west) =
  (leftBorderWidget, topleftJunction, topBorderWidget)
  where
    topBorder =
      UnicodeJunction.fromMaybeBold $ thisDayBold `maybeOr` north
    leftBorderOfNorth = -- the style of the left border of the northern neighbour
      UnicodeJunction.fromMaybeBold $ north `maybeOr` northwest
    topBorderOfWest = -- the style of the top border of the western neighbour
      UnicodeJunction.fromMaybeBold $ west `maybeOr` northwest
    leftBorder =
      UnicodeJunction.fromMaybeBold $ thisDayBold `maybeOr` west
    topBorderWidget =
      hBorder
      & UnicodeJunction.withLineType topBorder
      & forceAttr "cellBorder"
    leftBorderWidget =
      vBorder
      & UnicodeJunction.withLineType leftBorder
      & forceAttr "cellBorder"
    topleftJunction =
      str [UnicodeJunction.get
              leftBorderOfNorth topBorder
              leftBorder topBorderOfWest]
      & forceAttr "cellBorder"

renderRightmostBorder
  :: St n
  -- ^ the current state
  -> Day
  -- ^ the last day in the row
  -> Widget n
renderRightmostBorder st day =
  (j <=> lb)
  where
    boldDay = st^.focusDay
    thisDayBold = Just $ day == boldDay
    (Surrounding _ n' ne' _) =
      fmap ((==) boldDay) $ (st^.rowController.surroundingDays) day
    shifted_surroundings = Surrounding n' ne' Nothing thisDayBold
    (lb,j,_) = renderBorder st Nothing shifted_surroundings

scrollToFocus :: St n -> St n
scrollToFocus st = st
  --case L.find (\(_,(ds,_,_)) -> st^.focusDay `elem` ds) (zip [0..] $ st^.rows) of
  --  -- if the focus day is among the rows, but in the first row
  --  Just (0,(_,_,0)) ->
  --    -- focusDay is only fully visible if we do not have any scroll ofset
  --    if (st^.scrollOffset > 0)
  --    then st & scrollOffset .~ 0 & computeVisibleRows
  --    else st
  --  -- if the focus day is in the first row but this row is cropped at the bottom
  --  -- then the screen is too small and we can't do anything
  --  Just (0,(_,_,_)) ->
  --    st
  --  -- if the focus day is not in the first row (i.e. not cropped at the top)
  --  -- and not cropped from the bottom, then it's already fully visible
  --  Just (_,(_,_,0)) ->
  --    st
  --  -- if it is cropped at the bottom, then scroll to the top accordingly
  --  -- the 1 is for the southern border of the focused cell
  --  Just (_,(_,_,cb)) ->
  --    st & scrollOffset %~ (\o -> o + cb + 1) & computeVisibleRows
  --  -- if the focused day is not among the rows at all, then find out whether we
  --  -- need to go to the future or past
  --  Nothing ->
  --    if ((st^.focusDay) `diffDays` (st^.scrollDay) > 0)
  --    -- if focused day is in the future
  --    then st & scrollDay .~ (st^.focusDay)
  --            & scrollOffset .~ (daysHeight st (weekOf (st^.focusDay)) - (snd $ st^.size))
  --            & computeVisibleRows
  --    -- if focused day is in the past
  --    else st & scrollDay .~ st^.focusDay
  --            & scrollOffset .~ 0
  --            & computeVisibleRows

-- computeVisibleRows :: St n -> St n
-- computeVisibleRows st =
  --if (st^.scrollOffset < 0)
  --then st & scrollDay %~ (addDays (-7))
  --        & (\s -> s & scrollOffset %~ (\x -> x + weekHeight s))
  --        & computeVisibleRows
  --else
  --  if (st^.scrollOffset >= weekHeight st)
  --  then st & scrollDay %~ (addDays (7))
  --          & (\s -> s & scrollOffset %~ (\x -> x - weekHeight st))
  --          & computeVisibleRows
  --  else
  --  st & rows .~ new_rows_around (st^.scrollDay) (st^.scrollOffset)
  --where
  --  weekHeight s = daysHeight s (weekOf $ s^.scrollDay)
  --  addWeeksLater :: Int -> [Day] -> [([Day],Int,Int)]
  --  addWeeksLater space_remain week =
  --    if space_remain <= curheight
  --    then [(week, curheight, curheight - space_remain)]
  --    else
  --      (week, curheight, 0) : (addWeeksLater (space_remain - curheight) $ map (addDays 7) week)
  --    where curheight = daysHeight st week

  --  new_rows_around day offset =
  --    weekOf day
  --    & addWeeksLater ((snd $ st^.size) + offset)


weekOf :: Day -> [Day]
weekOf day =
  [1 - toInteger dayOfWeek .. 7 - toInteger dayOfWeek]
  & map (flip addDays day)
  where (_,_,dayOfWeek) = toWeekDate day

daysHeight :: St n -> Int -> [Day] -> Int
daysHeight s width ds = foldr max 1 $ map (dayHeight s width) ds

dayHeight :: St n -> Int -> Day -> Int
dayHeight st width d = max 4 (1 + (fst $ (st^.day2widget) d ((st^.rowController.dayWidth) d width)))

-- | scroll the viewport by terminal rows
scroll :: Int -> St n -> St n
scroll delta s = s
      & scrollOffset %~ ((+) delta)
      & normalizeScrollDay
      -- & computeVisibleRows

normalizeScrollDay :: St n -> St n
normalizeScrollDay st = st
    --where
    --    rows = computeVisibleRows st

-- | scroll the viewport by the given number of pages
scrollPage :: Double -> St n -> St n
scrollPage pages st =
    st
    -- & scroll (floor $ pages * (fromIntegral $ snd $ st^.size))
    -- & moveFocusIntoView


resize :: (Int,Int) -> St n -> St n
resize (w,h) s = s
    -- & size .~ (w,h)
    -- & computeVisibleRows
    -- & scrollToFocus


data Dir = DirUp | DirDown | DirLeft  | DirRight
dayInDirecton :: St n -> Dir -> Day
dayInDirecton st dir =
    flip addDays (st^.focusDay) $
    case dir of
        DirUp -> (-7)
        DirDown -> 7
        DirLeft -> (-1)
        DirRight -> 1

moveFocus :: Dir -> St n -> St n
moveFocus d st =
    st & focusDay .~ dayInDirecton st d
       & scrollToFocus


-- | update the focused cell s.t. it is in the view
moveFocusIntoView :: St n -> St n
moveFocusIntoView st = st
  --st & focusDay .~ fromMaybe (st^.focusDay) (do
  --  (first_row,_,_) <- listToMaybe (st^.rows)
  --  (last_row,_,_) <- lastSafe (st^.rows)
  --  first_day <- listToMaybe first_row
  --  last_day <- lastSafe last_row
  --  let (_,_,dayOfWeek) = toWeekDate (st^.focusDay)
  --  if (st^.focusDay < first_day) then
  --    return (addDays (toInteger dayOfWeek - 1) first_day)
  --  else if (st^.focusDay > last_day) then
  --      return (addDays (toInteger dayOfWeek - 7) last_day)
  --    else Nothing) -- nothing needs to be changed

gotoToday :: St n -> St n
gotoToday st =
    st -- & focusDay .~ (st^.today) & scrollToFocus

rangeVisible :: St n -> (Day,Day)
rangeVisible st =
    ( addDays (-80) (st^.scrollDay)
    , addDays 80 (st^.scrollDay)
    )
    -- $ do
    -- (first_row,_,_) <- listToMaybe (st^.rows)
    -- (last_row,_,_) <- lastSafe (st^.rows)
    -- first_day <- listToMaybe first_row
    -- last_day <- lastSafe last_row
    -- return (first_day, last_day)

resizeDays :: (Day -> DayWidget n) -> St n -> St n
resizeDays d st =
    st
    & day2widget .~ d
    -- & computeVisibleRows

