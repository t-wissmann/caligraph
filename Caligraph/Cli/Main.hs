{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr, cropTopBy, cropBottomBy,setAvailableSize,vBox,(<+>))
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (hCenter)

import qualified Caligraph.Cli.DayGrid as DayGrid
import Caligraph.Cli.DayGrid (Dir(DirUp,DirDown,DirLeft,DirRight))

import Caligraph.Utils
import qualified Caligraph.Config.Calendars as Config

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Text.Wrap
import Data.Array
import Data.Maybe
import qualified Caligraph.Cli.UnicodeJunction as UJ

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty (outputIface)
import Graphics.Vty.Output.Interface (supportsMode,Mode(Mouse),setMode)
import qualified Data.Map.Strict as Map
import qualified Caligraph.Backend as CB
import qualified Caligraph.Remind.Backend as Remind
import System.Exit
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)
import Data.List (sort, intersperse, isPrefixOf)

import Lens.Micro
import Lens.Micro.TH

data WidgetName =
    WNDayGrid
    | WNDay Day
    deriving (Ord,Eq,Show)

data St = St
    { _dayGrid :: DayGrid.St WidgetName
    , _backend :: CB.Backend
    , _visibleIncarnations :: Array Day [CB.Incarnation]
    }

makeLenses ''St


binds :: Map.Map ([Modifier],Key) (St -> EventM WidgetName (Next St))
binds = Map.fromList
  [ (([], KEsc), halt)
  , (([], KChar 'q'), halt)
  , (([], KChar 'o'), c $ DayGrid.gotoToday)

  , (([MCtrl], KChar 'd'), c $ DayGrid.scrollPage 0.45)
  , (([MCtrl], KChar 'u'), c $ DayGrid.scrollPage (-0.45))
  , (([MCtrl], KChar 'f'), c $ DayGrid.scrollPage 0.90)
  , (([MCtrl], KChar 'b'), c $ DayGrid.scrollPage (-0.90))

  -- hjkl
  , (([], KChar 'h'), focus_cmd DirLeft)
  , (([], KChar 'j'), focus_cmd DirDown)
  , (([], KChar 'k'), focus_cmd DirUp)
  , (([], KChar 'l'), focus_cmd DirRight)
  -- arrow keys
  , (([], KLeft ), focus_cmd DirLeft)
  , (([], KDown ), focus_cmd DirDown)
  , (([], KUp   ), focus_cmd DirUp)
  , (([], KRight), focus_cmd DirRight)
  ]
  where c f = (\st -> continue (st & dayGrid %~ f))


focus_cmd :: Dir -> St -> EventM WidgetName (Next St)
focus_cmd dir st =
    continue (st & dayGrid %~ DayGrid.moveFocus dir)


reminder2widget :: Int -> CB.Incarnation -> Int -> (Int, Widget n)
reminder2widget idx r width =
    ( length lines
    , updateAttrMap (applyAttrMappings [("reminder", mainAttribute)])
      $ withAttr "reminder"
      $ vBox
      $ map (\(d,s) -> (withAttr "reminderTime" $ str d) <+> txt s)
      $ zip placeholder lines
    )
  where
      mainAttribute =
        fg brightWhite
      --mainAttribute =
      --  brightWhite `on` (if idx `mod` 2 == 0 then rgbColor 161 0 168 else rgbColor 99 0 103)
      titleWidth = max 1 (width - durationWidth)
      lines =
        (\l -> l ++ replicate (length duration - length l) "")
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
    , updateAttrMap
            (applyAttrMappings
                [("reminder", mainAttribute)
                ])
      $ withAttr "reminder"
      $ vBox formatted_lines
    )
  where
      mainAttribute =
        fg brightWhite
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
day2widget :: St -> Day -> DayGrid.DayWidget WidgetName
day2widget st day width =
    (fromMaybe [] $ safeArray (st^.visibleIncarnations) day)
    & zipWith (\i d -> widget i d width) [0..]
    & intersperse (1, str $ replicate width ' ') -- put empty lines in between
    & (:) (1, str $ replicate width ' ') -- put an empty line below header
    & (:) headerWidget -- prepend header
    & flip (++) [(0, fixedfill ' ')] -- we do this to have empty space clickable
    & unzip
    & (\(a,b) -> (sum a, clickable (WNDay day) $ vBox b))
    where
      fixedfill :: Char -> Widget n
      fixedfill ch =
          Widget Fixed Fixed $ do
            c <- getContext
            return $ emptyResult & imageL .~ (V.charFill (c^.attrL) ch (c^.availWidthL) (c^.availHeightL))
      widget =
        if width < 20
        then reminder2widgetInline
        else reminder2widget
      today = st^.dayGrid^.DayGrid.today
      focus = st^.dayGrid^.DayGrid.focusDay
      headerAttr
        | day == focus && day == today = "cellHeaderFocusToday"
        | day == focus  = "cellHeaderFocus"
        | day == today  = "cellHeaderToday"
        | otherwise     = "cellHeader"
      (y,_,_) = toGregorian day
      (y_now,_,_) = toGregorian today
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
            $ formatTime defaultTimeLocale day_format day

ui st =
  [DayGrid.render $ st^.dayGrid]



tryEnableMouse :: EventM WidgetName ()
tryEnableMouse = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ do
      setMode output Mouse True
  return ()


mainApp :: App St () WidgetName
mainApp =
  App { appDraw = ui
      , appChooseCursor = const $ const Nothing
      , appHandleEvent = (\s ev ->
            do
            dg <- DayGrid.updateWidgetSize (s^.dayGrid)
            s' <- return (s & (dayGrid .~ dg))
            myHandleEvent s' ev
        )
      , appStartEvent = (\s -> tryEnableMouse >> return (updateDayRange s))
      , appAttrMap = const $ attrMap defAttr
        [ ("cellBorder", fg white)
        , ("cellHeader", yellow `on` black)
        , ("cellHeaderFocus", yellow `on` black)
        , ("cellHeaderToday", black `on` yellow)
        , ("cellHeaderFocusToday", black `on` yellow)
        , ("reminderTime", Attr (SetTo bold) (SetTo green) KeepCurrent)
        ]
      }

scrollStep = 3

myHandleEvent :: St -> BrickEvent WidgetName () -> EventM WidgetName (Next St)
myHandleEvent s (VtyEvent e) =
  case e of
    EvKey KEsc mods ->
      halt s
    EvKey key mods ->
      case Map.lookup (mods,key) binds of
        Just cb -> fmap (fmap updateDayRange) (cb s)
        Nothing -> continue (updateDayRange s)
    EvResize w h ->
      continue (s & dayGrid %~ DayGrid.resize (w,h) & updateDayRange)
    EvMouseDown _ _ BScrollDown _ ->
      continue (s & dayGrid %~ DayGrid.scroll scrollStep & updateDayRange)
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & dayGrid %~ DayGrid.scroll (-scrollStep) & updateDayRange)
    _ ->
      continue s
myHandleEvent s (AppEvent ()) = continue s
myHandleEvent s (MouseDown (WNDay d) BLeft _ _) =
      continue (s & dayGrid %~ DayGrid.setFocus d & updateDayRange)
myHandleEvent s (MouseDown _ BScrollDown _ _) =
      continue (s & dayGrid %~ DayGrid.scroll scrollStep & updateDayRange)
myHandleEvent s (MouseDown _ BScrollUp _ _) =
      continue (s & dayGrid %~ DayGrid.scroll (-scrollStep) & updateDayRange)
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s

updateDayRange :: St -> St
updateDayRange st =
    st
    & (if day_range == bounds (st^.visibleIncarnations)
      then id
      else visibleIncarnations .~ CB.query (st^.backend) day_range)
    & (\s -> s & dayGrid %~ (DayGrid.resizeDays $ day2widget s))
    where
    day_range = (DayGrid.rangeVisible $ st^.dayGrid)

testmain :: IO ()
testmain = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  today <- DayGrid.getToday
  args <- getArgs
  calendars <- Config.load >>= rightOrDie
  p <- (expandTilde $ Config.path $ snd $ calendars !! 0)
  backend <- Remind.init p
  customMain buildVty Nothing mainApp
    (St
        (DayGrid.init WNDayGrid today)
        backend
        (array (today,addDays (-1) today) []))
  return ()

rightOrDie :: Either String a -> IO a
rightOrDie = either die return

expandTilde :: FilePath -> IO FilePath
expandTilde s =
    case s of
     ['~'] -> getHomeDirectory
     ('~':'/':tl) -> do
        home <- getHomeDirectory
        return $ joinPath [home, tl]
     x -> return x

