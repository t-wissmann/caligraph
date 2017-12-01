{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Caligraph.Cli.DayWidget where

import Caligraph.Cli.Types
import Caligraph.Utils
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB

import Brick
import Brick.Main
import Brick.Widgets.Core (withAttr,vBox,(<+>))
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Center (hCenter)

import qualified Graphics.Vty as V
import Data.Time.Calendar
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T

import Lens.Micro
import Text.Wrap
import Data.List (intersperse)
import Data.Maybe
import Data.Array

data St = St
    { focus :: Maybe Int -- if this day has the keyboard focus
    , reminders :: [CB.Incarnation]
    , day :: Day
    , today :: Day
    }


reminder2widget :: Int -> CB.Incarnation -> Int -> (Int, Widget n)
reminder2widget idx r width =
    ( length lines
    , withAttr "reminderTitle"
      $ vBox
      $ map (\(d,s) -> (withAttr "reminderTime" $ str d) <+> txt s)
      $ zip placeholder lines
    )
  where
      --mainAttribute =
      --  brightWhite `on` (if idx `mod` 2 == 0 then rgbColor 161 0 168 else rgbColor 99 0 103)
      titleWidth = max 1 (width - durationWidth)
      lines =
        map (T.justifyLeft titleWidth ' ')
        $ (\l -> l ++ replicate (length duration - length l) "")
        $ wrapTextToLines
            (WrapSettings False True)
            titleWidth
            (T.pack $ CB.title r)
      durationWidth =
        if length duration == 0
        then 0
        else (+) 1 $ maximum $ map V.safeWcswidth duration

      placeholder =
        case duration of
            (beg:end:[]) ->
                ((beg++" ") : replicate (max 0 $ (length lines) - 2) "  |   ") ++ [end ++ " "]
            (beg:[]) ->
                ((beg++" ") : replicate (max 0 $ (length lines) - 1) "      ")
            _ ->
                (replicate (length lines) "")

      duration =
        case (CB.time r, CB.duration r) of
            (Just (h,m), Just (dh,dm)) ->
                let
                  m' = m + dm
                  h' = h + dh + m' `div` 60
                  -- c1 = UJ.get UJ.Empty  UJ.Strong UJ.Normal UJ.Strong
                  -- c2 = UJ.get UJ.Normal UJ.Strong  UJ.Empty UJ.Strong
                  c1 = ':'
                  c2 = ':'
                in
                [ CB.showTime c1 (h,m)
                , CB.showTime c2 (h' `mod` 24, m' `mod` 60)
                ]
            (Just (h,m), Nothing) ->
                [CB.showTime ':' (h,m) ]
            (_, _) -> []

reminder2widgetInline :: Int -> CB.Incarnation -> Int -> (Int, Widget n)
reminder2widgetInline idx r width =
    ( length formatted_lines
    , withAttr "reminderTitle"
      $ vBox formatted_lines
    )
  where
      -- bgcolor = (if idx `mod` 2 == 0 then rgbColor 161 0 168 else rgbColor 99 0 103)
      durationString =
        dropWhile (==' ') $
        case (CB.time r, CB.duration r) of
            (Just (h,m), Just (dh,dm)) ->
                let
                  m' = m + dm
                  h' = h + dh + m' `div` 60
                  -- c1 = UJ.get UJ.Empty  UJ.Strong UJ.Normal UJ.Strong
                  -- c2 = UJ.get UJ.Normal UJ.Strong  UJ.Empty UJ.Strong
                  c1 = ':'
                  c2 = ':'
                in
                CB.showTime c1 (h,m) ++ "-" ++
                CB.showTime c2 (h' `mod` 24, m' `mod` 60)
                ++ " "
            (Just (h,m), Nothing) ->
                CB.showTime ':' (h,m) ++ " "
            (_, _) -> ""
      raw_lines =
        map (T.unpack)
        $ map (T.justifyLeft width ' ')
        $ wrapTextToLines (WrapSettings False True) width
        $ T.pack
        $ (durationString ++ CB.title r)

      formatted_lines :: [Widget n]
      formatted_lines =
        case raw_lines of
            [] -> [str ""]
            (hd:tl) ->
                (:)
                    ((withAttr "reminderTime" $ str durationString)
                     <+>
                   (str "" <+> (str (drop (length durationString) hd))))
                    (map str tl)

-- | return a widget for a day and its total height
day2widget :: St -> DayWidget WidgetName
day2widget st width =
    (reminders st)
    & zipWith (\i d -> widget i d width) [0..]
    & intersperse (1, str $ replicate width ' ') -- put empty lines in between
    & (if (null $ reminders st) then (:) focusIndicator else id)
    & (:) (1, str $ replicate width ' ') -- put an empty line below header
    & (:) headerWidget -- prepend header
    & flip (++) [(0, fixedfill ' ')] -- we do this to have empty space clickable
    & unzip
    & (\(a,b) -> (sum a, clickable widgetName $ vBox b))
    where
      widgetName = (WNDay $ day st)
      fixedfill :: Char -> Widget n
      fixedfill ch =
          Widget Fixed Fixed $ do
            c <- getContext
            return $ emptyResult & imageL .~ (V.charFill (c^.attrL) ch (c^.availWidthL) (c^.availHeightL))

      widget :: Int -> CB.Incarnation -> Int -> (Int, Widget WidgetName)
      widget idx inc w=
        (if Just idx == (focus st)
        then (\(a,b) ->
            ( a
            , showCursor widgetName (Location (w-1,a-1))
                $ updateAttrMap
                    (mapAttrNames [ ("selectedReminderTime", "reminderTime")
                                  , ("selectedReminderTitle", "reminderTitle")
                                  ])
                $ b
            ))
        else id)
        $ (\(a,b) -> (a, clickable (WNDayItem (day st) idx) b))
        (if width < 20
        then reminder2widgetInline idx inc w
        else reminder2widget idx inc w)
      headerAttr
        | (isJust $ focus $ st) && (day st == today st) = "cellHeaderFocusToday"
        | (isJust $ focus $ st)  = "cellHeaderFocus"
        | (day st == today st)           = "cellHeaderToday"
        | otherwise              = "cellHeader"
      (y,_,_) = toGregorian (day st)
      (y_now,_,_) = toGregorian (today st)
      day_format
        | y == y_now    = "%d. %b"
        | width >= 12   = "%d. %b %Y"
        | width >= 10   = "%d. %b %y"
        | otherwise     = "%d-%m-%y"

      headerWidget =
        (,) 1
        $ withAttr headerAttr
            $ hCenter
            $ str
            $ formatTime defaultTimeLocale day_format (day st)

      -- if there is no reminder
      focusIndicator =
        if isJust (focus st)
        then (1, showCursor widgetName (Location (0, 0))
                 -- $ withAttr "selectedReminderTitle"
                 $ str
                 $ replicate width ' ')
        else (1, emptyWidget)

