{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr, cropTopBy, cropBottomBy,setAvailableSize)
import Brick.AttrMap (attrMap, AttrMap)

import qualified System.Console.Terminal.Size as TS

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
import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Lens.Micro
import Lens.Micro.TH

data State = State
  { _scrollOffset :: Int -- the number of rows hidden of _scrollDay
  , _scrollDay :: Day
  , _focusDay :: Day
  , _today :: Day
  , _size :: (Int,Int)
  , _rows :: [([Day],Int,Int)] -- the visible rows
  -- , _columns ::Int -- the number of days per row
  }

makeLenses ''State

ui :: State -> [Widget ()]
ui s =
  (s^.rows)
  & map (\(days,height,cb) ->
        map (dayWidget s) days
        & map (setAvailableSize (daywidth,height))
        & map (cropBottomBy cb)
        & hBox
        )
  & mapHead (cropTopBy (s^.scrollOffset))
  & vBox
  & (\x -> [x])
  where (fullwidth,_) = s^.size
        daywidth = fullwidth `div` 7
        mapHead _ [] = []
        mapHead f (x:xs) = (f x:xs)


prepareRows :: State -> State
prepareRows st =
  if (st^.scrollOffset < 0)
  then st & scrollDay %~ (addDays (-7))
          & (\s -> s & scrollOffset %~ (\x -> x + weekHeight s))
          & prepareRows
  else
    if (st^.scrollOffset >= weekHeight st)
    then st & scrollDay %~ (addDays (7))
            & (\s -> s & scrollOffset %~ (\x -> x - weekHeight st))
            & prepareRows
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

    weekOf :: Day -> [Day]
    weekOf day =
      [1 - toInteger dayOfWeek .. 7 - toInteger dayOfWeek]
      & map (flip addDays day)
      where (_,_,dayOfWeek) = toWeekDate day

    new_rows_around day offset =
      weekOf day
      & addWeeksLater ((snd $ st^.size) + offset)

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
  <=> str ("scrolloff: " ++ show (st^.scrollOffset))
  <=> str ("weeks: " ++ show (length (st^.rows)))
  <=> fill '.'

binds = Map.fromList
  [ (KEsc, halt)
  , (KChar 'q', halt)
  , (KChar 'o', gotoToday)

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
  continue (s & focusDay %~ addDays delta & prepareRows)

gotoToday s =
  continue (s & focusDay .~ (s^.today) & prepareRows)






stateToday :: (Int,Int) -> IO State
stateToday size = do
  g <- getCurrentTime
  let d = utctDay g
  return $ prepareRows $ State 0 d d d size []


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
      continue (s & size .~ (w,h) & prepareRows)
    EvMouseDown _ _ BScrollDown _ ->
      continue (s & scrollOffset %~ ((+) 2) & prepareRows)
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & scrollOffset %~ (\x -> (x - 2)) & prepareRows)
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
  termSize <- TS.size
  s <- stateToday (maybe (100,100) (\(TS.Window w h) -> (h,w)) termSize)
  customMain buildVty Nothing mainApp s
  return ()

