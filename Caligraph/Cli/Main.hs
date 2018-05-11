{-# LANGUAGE OverloadedStrings #-}

module Caligraph.Cli.Main where

import Brick
import Brick.Widgets.Border
import Brick.Main
import Brick.Widgets.Core (withAttr,vBox,(<+>))
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Widgets.Border.Style
import qualified Brick.Types as BT

import Caligraph.Cli.Types
import qualified Caligraph.Cli.DayGrid as DayGrid
import Caligraph.Cli.AppState
import qualified Caligraph.Cli.DayWidget as DayWidget

import Caligraph.Utils
import Caligraph.Possibly
import qualified Caligraph.Config.Calendars as Config
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Calendar as CC

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Control.Monad.IO.Class (liftIO)
import Data.Array
import Data.Maybe
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
add_reminder_cmd = do
    day <- use (dayGrid . DayGrid.focusDay)
    let title = "New Reminder"
    zoom calendar $ CC.addReminder $ CB.PartialReminder day title Nothing Nothing Nothing

dequeueIO :: MonadIO io => StateT St io ()
dequeueIO = do
    c <- use calendar
    case CC.dequeueIO c of
        Nothing -> return ()
        Just io_action -> do
            c' <- liftIO $ io_action
            calendar .= c'
            updateDayRange' True
            -- fix focusItem
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
    return $ fromMaybe [] $ safeArray visibInc day

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
        , ("statusline", blue `on` black)
        , ("reminderTitle", defAttr)
        , ("reminderTime", Attr (SetTo bold) (SetTo green) KeepCurrent)
        , ("selectedReminderTitle", bg black)
        , ("selectedReminderTime", Attr (SetTo bold) (SetTo green) (SetTo black))
        ]
      }

scrollStep = 3

continueOrHalt :: St -> EventM wn (Next St)
continueOrHalt s =
    if s^.aboutToQuit
    then halt s
    else continue s

myHandleEvent :: St -> BrickEvent WidgetName () -> EventM WidgetName (Next St)
myHandleEvent s (VtyEvent e) =
  case e of
    EvKey key mods ->
      case Map.lookup (mods,key) binds of
        Just cmd ->
            let Breakpoint io_action = runStateT (do cmd ; dequeueIO) s in
            do
                possibly_io <- liftIO $ io_action
                case possibly_io of
                    Pure ((),s') ->
                        continueOrHalt (updateDayRange s')
                    Monadic fg_io_action ->
                        suspendAndResume $
                            fmap (updateDayRange . snd) fg_io_action
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
            (if focus == day
              then Just $ fromMaybe (length reminders - 1) (st^.focusItem)
              else Nothing)
            reminders
            day
            today)
    where
      today = st^.dayGrid^.DayGrid.today
      focus = st^.dayGrid^.DayGrid.focusDay
      reminders = (fromMaybe [] $ safeArray (st^.visibleIncarnations) day)

updateDayRange :: St -> St
updateDayRange = execState $ updateDayRange' False

embed :: Monad m => State s r -> StateT s m r
embed = mapStateT (return . runIdentity)

updateDayRange' :: Monad m => Bool -> StateT St m ()
updateDayRange' force = do
    day_range <- fmap DayGrid.rangeVisible $ use dayGrid
    incs <- use visibleIncarnations
    if (day_range == bounds incs) && not force
      then return ()
      else do
        incs' <- zoom calendar $ embed $ CC.query day_range
        visibleIncarnations .= incs'
    s <- get
    dayGrid %= (DayGrid.resizeDays $ day2widget s)

testmain :: IO ()
testmain = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  today <- DayGrid.getToday
  args <- getArgs
  raw_calendars <- Config.load >>= rightOrDie
  cal <- rightOrDie $ CC.fromConfig (snd $ raw_calendars !! 0)
  cal_loaded <-
    case CC.dequeueIO cal of
        Nothing -> return cal
        Just io_action -> io_action
  customMain buildVty Nothing mainApp
    (AppState
        False
        (DayGrid.init WNDayGrid today)
        (array (today,addDays (-1) today) [])
        (Just 0)
        cal_loaded
        ["Caligraph started"])
  return ()

rightOrDie :: Either String a -> IO a
rightOrDie = either die return

