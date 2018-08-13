{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.BChan
import Brick.Widgets.Core (withAttr,vBox,(<+>))
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit as Brick
import qualified Brick.Types as BT

import Caligraph.Cli.Types
import qualified Caligraph.Cli.DayGrid as DayGrid
import Caligraph.Cli.AppState
import qualified Caligraph.Cli.DayWidget as DayWidget

import Caligraph.Utils
import Caligraph.Possibly
import qualified Caligraph.Config.Calendars as Config
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Caligraph.Calendar as CC

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Control.Monad.IO.Class (liftIO)
import Data.Array
import Data.Maybe
import qualified Data.List as L
import Data.Semigroup
import Data.Functor.Identity
import qualified Caligraph.Cli.UnicodeJunction as UJ

import qualified Data.Text as T
import Data.Time.Calendar
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty (outputIface)
import Graphics.Vty.Output.Interface (supportsMode,Mode(Mouse),setMode)
import qualified Data.Map.Strict as Map
import System.Exit
import System.Environment (getArgs)

import Debug.Trace

import Lens.Micro
import Lens.Micro.Mtl

type St =  Caligraph.Cli.AppState.AppState

data ExternalEvent = CalendarIO

data CmdOutput = CmdOutput
    { cmdoutStderr :: [String]
    , cmdoutSuspendGui :: Max Bool
    }

instance Monoid CmdOutput where
    mempty = CmdOutput mempty mempty
    mappend (CmdOutput a1 a2) (CmdOutput b1 b2) =
        CmdOutput (mappend a1 b1) (mappend a2 b2)


data FgIO a = NoFg a | WithFg (IO a)
data BgFgIO a = BgFgIO (IO (FgIO a))

data Breakpoint m a = Breakpoint {
    projBreakpoint :: (m (Possibly m a))
  }

discardBreakpoint :: Monad m => Breakpoint m a -> m a
discardBreakpoint (Breakpoint mx) = do
    x <- mx
    case x of
        Pure y -> return y
        Monadic my -> my

instance Functor m => Functor (Breakpoint m) where
    fmap f (Breakpoint mx) = Breakpoint (fmap (fmap f) mx)

instance Applicative m => Applicative (Breakpoint m) where
    pure = Breakpoint . pure . pure
    Breakpoint f <*> Breakpoint x = Breakpoint (t2 f x)
        where
          t2 :: (Applicative f2, Applicative f1) => f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
          t2 = (<*>) . ((<*>) (pure (<*>)))

instance Monad m => Monad (Breakpoint m) where
    (Breakpoint mx) >>= f = Breakpoint $ do
        -- do in the monad m
        possibly_mx <- mx
        case possibly_mx of
            Pure x -> projBreakpoint (f x)
            Monadic mx' ->
                return $ Monadic $ do
                    x <- mx'
                    discardBreakpoint (f x)

instance MonadTrans Breakpoint where
    lift action = Breakpoint (fmap return action)

instance MonadIO m => MonadIO (Breakpoint m) where
    liftIO io_action = Breakpoint (fmap return $ liftIO io_action)

breakMonad :: Monad m => Breakpoint m ()
breakMonad = Breakpoint $ return $ Monadic $ return ()

--instance Monad BgFgIO where
--  return = BgFgIO
--  (B


type Cmd st = StateT st (Breakpoint IO) ()

requestSuspendGui = lift breakMonad

binds :: Map.Map ([Modifier],Key) (Cmd St)
binds = Map.fromList
  [ (([], KEsc), quit_cmd)
  , (([], KChar 'q'), quit_cmd)
  , (([], KChar 'e'), edit_externally_cmd)
  , (([], KChar 'a'), add_reminder_cmd)
  , (([], KChar 'o'), dayGrid %= DayGrid.gotoToday)

  , (([MCtrl], KChar 'd'), dayGrid %= DayGrid.scrollPage 0.45)
  , (([MCtrl], KChar 'u'), dayGrid %= DayGrid.scrollPage (-0.45))
  , (([MCtrl], KChar 'f'), dayGrid %= DayGrid.scrollPage 0.90)
  , (([MCtrl], KChar 'b'), dayGrid %= DayGrid.scrollPage (-0.90))

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

quit_cmd :: Cmd St
quit_cmd =
    aboutToQuit .= True

focus_cmd :: Dir -> Cmd St
focus_cmd dir = do
    focus <- use $ dayGrid . DayGrid.focusDay
    reminders <- getReminders focus
    fi <- use focusItem
    let focusItemConcrete = case fi of
                            Nothing -> length reminders - 1
                            Just idx -> idx
    case reminderInDir focusItemConcrete reminders of
        Right idx ->
            focusItem .= Just idx
        Left idx -> do
            focusItem .= idx
            dayGrid %= DayGrid.moveFocus dir
    where
      reminderInDir :: Int -> [CB.Incarnation'] -> Either (Maybe Int) Int
      reminderInDir focusItemConcrete reminders =
        case dir of
            DirLeft -> Left (Just 0)
            DirRight -> Left (Just 0)
            DirUp ->
                if focusItemConcrete > 0
                then Right $ focusItemConcrete - 1
                else Left Nothing
            DirDown ->
                if focusItemConcrete + 1 < length reminders
                then Right $ focusItemConcrete + 1
                else Left (Just 0)

edit_externally_cmd :: Cmd St
edit_externally_cmd = do
    day <- use (dayGrid . DayGrid.focusDay)
    rems <- getReminders day
    idx <- fmap (fromMaybe $ length rems - 1) $ use focusItem
    if idx >= 0 && idx < length rems
    then do
        let p = CB.itemId $ rems !! idx
        requestSuspendGui
        zoom calendar $ CC.editExternally p
    else return ()

add_reminder_cmd :: Cmd St
add_reminder_cmd =
    mode .= AMAppend

addReminderFromString :: String -> Cmd St
addReminderFromString "" = return ()
addReminderFromString buf = do
    day <- use (dayGrid . DayGrid.focusDay)
    let (from,duration,title) = CB.parseTimeDuration buf
    let pr = CB.PartialReminder day title from duration Nothing
    zoom calendar $ embed $ CC.addReminder $ pr

fileIOQueries :: MonadIO io => StateT St io ()
fileIOQueries = do
    log <- zoom calendar $ CC.fileQuery
    case log of
        Just m -> messages %= (:) m
        Nothing -> return ()


fixFocusItem :: Monad m => StateT St m ()
fixFocusItem = do
    updateDayRange' True
    day <- use $ dayGrid . DayGrid.focusDay
    reminders <- getReminders day
    focusItem %= fmap (min $ length reminders - 1)

ui st =
  [DayGrid.render (st^.dayGrid) <=> footer]
  where
    footer = withAttr "statusline"
        $ vBox
        $ map (padRight BT.Max . str)
        $ reverse $ take 5 (st^.messages)

getReminders :: Monad m => Day -> StateT St m [CB.Incarnation']
getReminders day = do
    visibInc <- use visibleIncarnations
    return $ L.sort $ fromMaybe [] $ safeArray visibInc day

tryEnableMouse :: EventM WidgetName ()
tryEnableMouse = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ do
      setMode output Mouse True
  return ()


mainApp :: App St ExternalEvent WidgetName
mainApp =
  App { appDraw = ui
      , appChooseCursor = showFirstCursor
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
        , ("dayOfWeek", magenta `on` black)
        , ("statusline", blue `on` black)
        , ("reminderTitle", defAttr)
        , ("reminderTime", Attr (SetTo bold) (SetTo green) KeepCurrent)
        , ("selectedReminderTitle", bg black)
        , (Brick.editFocusedAttr, bg black)
        , ("selectedReminderTime", Attr (SetTo bold) (SetTo green) (SetTo black))
        ]
      }

scrollStep = 3

continueOrHalt :: St -> EventM wn (Next St)
continueOrHalt s =
    if s^.aboutToQuit
    then halt s
    else continue s

execCmd :: St -> Cmd St -> EventM WidgetName (Next St)
execCmd s cmd =
    let Breakpoint io_action = runStateT (do cmd ; fileIOQueries) s in
    do
        possibly_io <- liftIO $ io_action
        case possibly_io of
            Pure ((),s') ->
                continueOrHalt (updateDayRange s')
            Monadic fg_io_action ->
                suspendAndResume $
                    fmap (updateDayRange . snd) fg_io_action

myHandleEvent :: St -> BrickEvent WidgetName ExternalEvent -> EventM WidgetName (Next St)
myHandleEvent s (VtyEvent e) =
  case e of
    EvKey key mods ->
      case (s^.mode) of
        AMAppend ->
          case (key,mods) of
            (KEsc,[]) ->
              continue (s & mode .~ AMNormal & updateDayRange)
            (KEnter,[]) ->
              execCmd s $ do
                  editor <- use newReminderEditor
                  newReminderEditor .= emptyReminderEditor
                  mode .= AMNormal
                  addReminderFromString (head (getEditContents editor))
            _ ->
              continue =<< fmap updateDayRange (handleEventLensed s newReminderEditor Brick.handleEditorEvent (EvKey key mods))
        AMNormal ->
          case Map.lookup (mods,key) binds of
            Just cmd ->
                execCmd s cmd
            Nothing -> continue (updateDayRange s)
    EvResize w h ->
      continue (s & dayGrid %~ DayGrid.resize (w,h) & updateDayRange)
    EvMouseDown _ _ BScrollDown _ ->
      continue (s & dayGrid %~ DayGrid.scroll scrollStep & updateDayRange)
    EvMouseDown _ _ BScrollUp _ ->
      continue (s & dayGrid %~ DayGrid.scroll (-scrollStep) & updateDayRange)
    _ ->
      continue s
myHandleEvent s (AppEvent ev) =
    case ev of
        CalendarIO -> do
            s' <- flip execStateT s $ do
                zoom calendar CC.receiveResult
                log <- zoom calendar $ CC.fileQuery
                case log of
                    Just m -> messages %= (:) m
                    Nothing -> return ()
                c <- use calendar
                incs <- use visibleIncarnations
                visibleIncarnations .= CC.cachedIncarnations c (bounds incs)
                s'' <- get
                dayGrid %= (DayGrid.resizeDays $ day2widget s'')
            continue s'
myHandleEvent s (MouseDown (WNDay d) BLeft _ _) =
      continue (s & dayGrid %~ DayGrid.setFocus d & focusItem .~ Just 0 & updateDayRange)
myHandleEvent s (MouseDown (WNDayItem d idx) BLeft _ _) =
      continue (s & dayGrid %~ DayGrid.setFocus d & focusItem .~ Just idx & updateDayRange)
myHandleEvent s (MouseDown _ BScrollDown _ _) =
      continue (s & dayGrid %~ DayGrid.scroll scrollStep & updateDayRange)
myHandleEvent s (MouseDown _ BScrollUp _ _) =
      continue (s & dayGrid %~ DayGrid.scroll (-scrollStep) & updateDayRange)
myHandleEvent s (MouseDown _ _ _ _) = continue s
myHandleEvent s (MouseUp _ _ _) = continue s

day2widget :: St -> Day -> DayWidget WidgetName
day2widget st day =
    DayWidget.day2widget
        (DayWidget.St
            (if focus == day && (st^.mode) == AMNormal
              then Just $ fromMaybe (length reminders - 1) (st^.focusItem)
              else Nothing)
            reminders
            day
            today
            (case (st^.mode, focus==day) of
                (AMAppend,True) -> Just (st^.newReminderEditor)
                _ -> Nothing))
    where
      today = st^.dayGrid^.DayGrid.today
      focus = st^.dayGrid^.DayGrid.focusDay
      reminders = L.sort (fromMaybe [] $ safeArray (st^.visibleIncarnations) day)

updateDayRange :: St -> St
updateDayRange = execState $ do
    m <- use mode
    updateDayRange' (m == AMAppend)

updateDayRange' :: Monad m => Bool -> StateT St m ()
updateDayRange' force = do
    -- get currently visible day range
    day_range <- fmap DayGrid.rangeVisible $ use dayGrid

    incs <- use visibleIncarnations
    if (day_range == bounds incs) && not force
       then return ()
       else do
         -- report it to the calendar
         zoom calendar $ embed $ CC.setRangeVisible day_range
         c <- use calendar
         visibleIncarnations .= CC.cachedIncarnations c day_range
    s <- get
    dayGrid %= (DayGrid.resizeDays $ day2widget s)

emptyReminderEditor :: Brick.Editor String WidgetName
emptyReminderEditor =
    (Brick.editor WNNewReminder (Just 1) "")

testmain :: IO ()
testmain = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  today <- DayGrid.getToday
  args <- getArgs
  raw_calendars <- Config.load >>= rightOrDie
  chan <- newBChan (1 + length raw_calendars)
  cal <- rightOrDie $ CC.fromConfig (writeBChan chan CalendarIO) (snd $ raw_calendars !! 0)
  cal' <- cal
  let day_grid = (DayGrid.init WNDayGrid today)
  let day_range = DayGrid.rangeVisible day_grid
  (msg, cal_loaded) <- runStateT (do embed (CC.setRangeVisible day_range); CC.fileQuery) cal'
  let log = maybeToList msg
  customMain buildVty (Just chan) mainApp
    (AppState
        False
        day_grid
        (array (today,addDays (-1) today) [])
        (Just 0)
        cal_loaded
        log
        AMNormal
        emptyReminderEditor
        )
  return ()

rightOrDie :: Either String a -> IO a
rightOrDie = either die return

