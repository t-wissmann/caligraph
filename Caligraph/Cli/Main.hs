{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr)
import Brick.AttrMap (attrMap, AttrMap)
import Data.Time.Calendar
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Map.Strict as Map

import Lens.Micro
import Lens.Micro.TH

data State = State
  { _scrollOffset :: Int
  , _focusDay :: Day
  , _today :: Day
  }

makeLenses ''State

ui :: State -> [Widget ()]
ui s =
  [days]
  where
    (_,_,dayOfWeek) = toWeekDate (s^.focusDay)
    days =
      [1 - toInteger dayOfWeek .. 7 - toInteger dayOfWeek]
      & map (flip addDays (s^.focusDay))
      & map (\d -> dayWidget d (d == (s^.today)) (d == (s^.focusDay)))
      & foldr1 (\x y -> x <+> vBorder <+> y)

dayWidget :: Day -> Bool -> Bool -> Widget ()
dayWidget day isToday isFocused =
  (if isFocused then withAttr "focusDay" else id) $
  (if isToday then withAttr "today" else id) $
  str (show day)

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
  return $ State 0 d d

mainApp :: App State () ()
mainApp =
  App { appDraw = ui
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = myHandleEvent
      , appStartEvent = return
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
    _ ->
      continue s

myHandleEvent s (AppEvent ()) = continue s
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s


testmain :: IO ()
testmain = do
  s <- stateToday
  defaultMain mainApp s
  return ()

