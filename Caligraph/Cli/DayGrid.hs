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
import Control.Exception.Base
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Extras (view)
import Data.Functor.Const

validate_assert :: (a -> (String,Bool)) -> a -> a
validate_assert pred x =
  case (pred x) of
    (_, True) -> x
    (str, False) -> error ("Assertion failed: " ++ str)

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
  , _dayWidth = (\d fullwidth -> (fullwidth - 1) `div` 7)
  }

-- | The internal state
data St n = St
  { _widgetName :: n
  , _scrollOffset :: Int -- the number of rows hidden of _scrollDay
  , _scrollDay :: Day
  , _focusDay :: Day
  , _today :: Day
  , _size :: Maybe (Int,Int)
  , _day2widget :: Day -> (DayWidget n)
  , _dayCache :: Map Day (DayWidget n)
  , _rowController :: RowController
  -- , _columns ::Int -- the number of days per row
  }

makeLenses ''St

type StDays n = LazyResult Day (DayWidget n) (St n)

data ScreenRow = ScreenRow
  { srDays :: [Day]
  , srHeight :: Int
  , srCropTop :: Int
  , srCropBottom :: Int
  }


init
  :: n
  -- ^ the widgets name
  -> Day
  -- ^ today
  -> St n
init n d =
  St n 0 d d d Nothing (const emptyDay) M.empty weekPerRow

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
  reportExtent (st^.widgetName) $
  Widget Greedy Greedy render'
  where
    rc = (st^.rowController)
    mapHead _ [] = []
    mapHead f (x:xs) = (f x:xs)
    render' = do
      fullwidth <- asks (view availWidthL)
      height <- asks (view availHeightL)
      Brick.Types.render $
        computeVisibleRows st (fullwidth,height)
        & map (renderRow fullwidth)
        & vBox
    -- renderRow :: ScreenRow -> Int -> Widget ''n
    renderRow fullwidth (ScreenRow days height ct cb) =
      days
      & map (\d -> ((rc^.dayWidth) d fullwidth, d))
      & map (\(width,d) -> (width, snd $ renderDay st width d))
      & flip (++) [(1, renderRightmostBorder st $ last days)]
      & map (\(width,d) -> setAvailableSize (width,height) d)
      & map (cropTopBy ct)
      & map (cropBottomBy cb)
      & hBox

-- | call this after this widget has been rendered
updateWidgetSize :: Eq n => St n -> EventM n (St n)
updateWidgetSize st = do
  mExtent <- lookupExtent (st^.widgetName)
  case mExtent of
    Nothing -> return st
    Just (Extent _ _ lastsize _) ->
      st
      & size .~ Just lastsize
      & return

computeVisibleRows
    :: St n
    -> (Int,Int)
    -- ^ the screen size
    -> [ScreenRow]
    -- ^ the list of visible rows, containing their height and the number
    --   of terminal rows cropped off at the bottom
computeVisibleRows st (fullwidth,fullheight) =
  -- validate_assert (test_computeVisibleRows fullheight) $
  reverse (rowsBefore firstrow ((-1) * (st^.scrollOffset)))
  ++ remainingRows firstrow (max 0 (st^.scrollOffset)) (fullheight+ (st^.scrollOffset))
  where
    rc = (st^.rowController)
    firstrow = (rc^.rowOf) (st^.scrollDay)
    -- return a reversed list of rows (properly) on top of the given row
    rowsBefore :: [Day] -> Int -> [ScreenRow]
    rowsBefore row rem_space =
      let rowB = (rc^.previousRow) row in
      let rowHeight = daysHeight st fullwidth rowB in
      if rem_space <= 0 then
        []
      else
        (ScreenRow rowB rowHeight (max 0 (rowHeight - rem_space)) 0)
        : rowsBefore rowB (rem_space - rowHeight)

    remainingRows :: [Day] -> Int -> Int -> [ScreenRow]
    remainingRows row crop_top rem_height =
      let row_height = daysHeight st fullwidth row in
      if rem_height <= row_height
      then [ScreenRow row row_height crop_top (row_height - rem_height)]
      else
        (ScreenRow row row_height crop_top 0)
        : remainingRows
            (st^.rowController.nextRow $ row)
            0
            (rem_height - row_height)

test_computeVisibleRows :: Int -> [ScreenRow] -> (String,Bool)
test_computeVisibleRows fullheight rows =
  (,) "computeVisibleRows" $
  rows
  & map (\(ScreenRow _ h t b) -> h - t - b)
  & sum
  & (==) fullheight

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
scrollToFocus st = scrollToDay st (st^.focusDay)

scrollToDay :: St n -> Day -> St n
scrollToDay st day =
  -- TODO: query additionaly day content if the jump is too far
  case (st^.size) of
    Nothing -> st
    Just (width,height) ->
      let rows = computeVisibleRows st (width,height) in
      case L.find (\sr -> day `elem` srDays sr) rows of
        -- if the day is among the rows
        Just (ScreenRow _ _ ct cb) ->
          -- desired day is only fully visible if it is not cropped at all
          st & scrollOffset %~ (\o ->
                  if (cb /= 0)
                  then
                    -- not only scroll by the amount cropped
                    -- but additionally by one line to make the bottom border visible
                    -- (the bottom border is part of the cell below)
                    o + cb + 1
                  else o - ct)
             & normalizeScrollDay
        -- if the day is not among the rows at all, then find out whether we
        -- need to go to the future or past
        Nothing ->
          if (day `diffDays` (st^.scrollDay) > 0)
          -- if focused day is in the future
          then
            let
              weekHeight = daysHeight st width ((st^.rowController.rowOf) day)
              -- the + 1 is for the bottom border of the cell
              newOffset = (weekHeight - height + 1)
            in
            st & scrollDay .~ day
               & scrollOffset .~ newOffset
               & normalizeScrollDay
          -- if focused day is in the past
          else st & scrollDay .~ day
                  & scrollOffset .~ 0
                  & normalizeScrollDay

weekOf :: Day -> [Day]
weekOf day =
  [1 - toInteger dayOfWeek .. 7 - toInteger dayOfWeek]
  & map (flip addDays day)
  where (_,_,dayOfWeek) = toWeekDate day

minimumDayHeight = 4

-- | the height of a list of days
daysHeight
  :: St n
  -- ^ internal state
  -> Int
  -- ^ the full width of the daygrid widget
  -> [Day]
  -- ^ the day in question
  -> Int
  -- ^ day's height, including its top border
daysHeight s width ds = foldr max minimumDayHeight $ map (dayHeight s width) ds

-- | the height of a specific day
dayHeight
  :: St n
  -- ^ internal state
  -> Int
  -- ^ the full width of the daygrid widget
  -> Day
  -- ^ the day in question
  -> Int
  -- ^ day's height, including its top border
dayHeight st width d =
  -- (\x -> trace (show d ++ " -> height " ++ show x) x) $
  -- substract one column for the left border
  -- and add one row for the top border
  max minimumDayHeight (1 + (fst $ (st^.day2widget) d ((st^.rowController.dayWidth) d width - 1)))

-- | scroll the viewport by terminal rows
scroll :: Int -> St n -> St n
scroll delta s = s
      & scrollOffset %~ ((+) delta)
      & normalizeScrollDay

normalizeScrollDay :: St n -> St n
normalizeScrollDay st =
  case (st^.size) of
    Nothing -> st
    Just (width,height) ->
      let firstRowHeight = daysHeight st width firstRow in
      let prevRow = (rc^.previousRow) firstRow in
      if (st^.scrollOffset < 0) then
        st
        & scrollDay .~ head prevRow
        & scrollOffset %~ (+) (daysHeight st width prevRow)
        & normalizeScrollDay
      else if (st^.scrollOffset >= firstRowHeight) then
        st
        & scrollOffset %~ flip (-) firstRowHeight
        & scrollDay .~ (head $ (rc^.nextRow) firstRow)
        & normalizeScrollDay
      else st
    where
        rc = (st^.rowController)
        firstRow = (rc^.rowOf) (st^.scrollDay)


-- | scroll the viewport by the given number of pages
scrollPage :: Double -> St n -> St n
scrollPage pages st =
  case (st^.size) of
    Nothing -> st
    Just (_,height) ->
      st
      & scroll (floor $ pages * (fromIntegral height))
    -- & moveFocusIntoView


resize :: (Int,Int) -> St n -> St n
resize (w,h) s = s
    & size .~ Just (w,h)
    & normalizeScrollDay
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


setFocus :: Day -> St n -> St n
setFocus d st =
    st & focusDay .~ d

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
    st & focusDay .~ (st^.today) & scrollToFocus

rangeVisible :: St n -> (Day,Day)
rangeVisible st =
    let
      height =
        case (st^.size) of
          Nothing -> 1000
          Just (_,h) -> h
      daysVisible = fromIntegral $
        (st^.rowController.columns)
          * (height `div` minimumDayHeight)
    in
    ( addDays (-80) (st^.scrollDay)
    , addDays (80+daysVisible) (st^.scrollDay)
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

-- vim: sw=2 ts=2
