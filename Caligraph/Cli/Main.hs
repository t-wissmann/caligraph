{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr, cropTopBy, cropBottomBy,setAvailableSize)
import Brick.AttrMap (attrMap, AttrMap)

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.Time.Calendar
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty (outputIface)
import Graphics.Vty.Output.Interface (supportsMode,Mode(Mouse),setMode)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Map.Strict as Map

import Lens.Micro
import Lens.Micro.TH

data State = State
  { _scrollOffset :: Int -- the row of the focused day
  , _focusDay :: Day
  , _today :: Day
  , _size :: (Int,Int)
  , _rows :: [(Int,[Day],Int)] -- the visible rows
  }

makeLenses ''State

ui :: State -> [Widget ()]
ui s =
  (s^.rows)
  & map (\(ct,days,cb) ->
        map (dayWidget s) days
        & map (setAvailableSize (daywidth,daysHeight s days))
        & foldr1 (\x y -> x <+> vBorder <+> y)
        & cropTopBy ct
        & cropBottomBy cb
        )
  & vBox
  & (\x -> [x])
  where (fullwidth,_) = s^.size
        daywidth = (fullwidth - 8) `div` 7


prepareRows :: State -> State
prepareRows st =
  (st & rows .~ new_rows)
  where
    (_,_,dayOfWeek) = toWeekDate (st^.focusDay)
    focusedWeek :: [Day]
    focusedWeek =
      [1 - toInteger dayOfWeek .. 7 - toInteger dayOfWeek]
      & map (flip addDays (st^.focusDay))

    addWeeksLater :: Int -> [Day] -> [([Day],Int)]
    addWeeksLater space_remain week =
      if space_remain <= 0
      then [(week, -space_remain)]
      else
        (week, 0) : (addWeeksLater (space_remain - daysHeight st week) $ map (addDays 7) week)

    addWeeksBefore :: Int -> [([Day],Int)] -> [(Int,[Day],Int)]
    addWeeksBefore cur_scroll_offset ((week,cropBot):later) =
      if (cur_scroll_offset <= 0)
      then ((-cur_scroll_offset,week,cropBot) : map (\(x,y) -> (0,x,y)) later)
      else
        addWeeksBefore new_scroll_offset ((weekBefore,0):(week,cropBot):later)
        where
            weekBefore = map (addDays (-7)) week
            weekHeight = daysHeight st weekBefore
            new_scroll_offset = cur_scroll_offset - weekHeight

    new_rows =
      focusedWeek
      & addWeeksLater ((fst $ st^.size) - (st^.scrollOffset))
      & addWeeksBefore (st^.scrollOffset)

daysHeight :: State -> [Day] -> Int
daysHeight s ds = foldr max 0 $ map (dayHeight s) ds

dayHeight :: State -> Day -> Int
dayHeight _ _ = 20


dayWidget :: State -> Day -> Widget ()
dayWidget st day =
  (if day == (st^.focusDay) then withAttr "focusDay" else id) $
  (if day == (st^.today) then withAttr "today" else id) $
  hBorder
  <=> str (show day)
  <=> str (show (st^.scrollOffset))
  <=> vBox (map (str.show) [1..30])

binds = Map.fromList
  [ (KEsc, halt)
  , (KChar 'q', halt)

  -- hjkl
  , (KChar 'h', switchDay (-1))
  , (KChar 'j', switchDay  7)
  , (KChar 'k', switchDay (-7))
  , (KChar 'l', switchDay  1)
  -- arrow keys
  , (KLeft , switchDay (-1))
  , (KDown , switchDay  7)
  , (KUp   , switchDay (-7))
  , (KRight, switchDay  1)
  ]

switchDay delta s =
  continue (s & focusDay %~ addDays delta)



stateToday :: IO State
stateToday = do
  g <- getCurrentTime
  let d = utctDay g
  return $ prepareRows $ State 0 d d (100,100) []


tryEnableMouse :: EventM n ()
tryEnableMouse = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ do
      setMode output Mouse True
  return ()


mainApp :: App State () ()
mainApp =
  App { appDraw = ui
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = myHandleEvent
      , appStartEvent = (\s -> tryEnableMouse >> return s)
      , appAttrMap = const $ attrMap defAttr
        [ ("today", fg red)
        , ("focusDay", bg black)
        ]
      }

myHandleEvent s (VtyEvent e) =
  case e of
    EvKey KEsc mods ->
      halt s
    EvKey key mods ->
      case Map.lookup key binds of
        Just cb -> cb s
        Nothing -> continue s
    EvResize w h ->
      continue (s & size .~ (w,h))
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & scrollOffset %~ ((+) 1))
    EvMouseDown _ _ BScrollDown _ ->
      continue (s & scrollOffset %~ \x -> (x - 1))
    _ ->
      continue s

myHandleEvent s (AppEvent ()) = continue s
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s


testmain :: IO ()
testmain = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  s <- stateToday
  customMain buildVty Nothing mainApp s
  return ()

