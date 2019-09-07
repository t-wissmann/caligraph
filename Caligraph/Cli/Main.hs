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
import Caligraph.Breakpoint
import Caligraph.Possibly
import Caligraph.PointerStore (Ptr)
import qualified Caligraph.Config.Main as MainConfig
import qualified Caligraph.Config.Types as CfgTypes
import Caligraph.Config.Types (userShow)
import qualified Caligraph.Config.Defaults as ConfigDefaults
import qualified Caligraph.Config.Command as CCommand
import qualified Caligraph.Config.Calendars as CalendarConfig
import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB
import qualified Caligraph.Calendar as CC
import qualified Caligraph.Rules as Rules

import Control.Monad (when)
import Control.Monad.Loops (whileM_)
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Array
import Data.Maybe
import Data.Fixed (Fixed(MkFixed),resolution)
import qualified Data.List as L
import Data.Semigroup
import Data.Functor.Identity
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone,utcToLocalTime,LocalTime(LocalTime),TimeOfDay(TimeOfDay))
import qualified Caligraph.Cli.UnicodeJunction as UJ

import qualified Data.Text as T
import Text.Printf (printf)
import Data.Time.Calendar
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty (outputIface)
import Graphics.Vty.Output.Interface (supportsMode,Mode(Mouse),setMode)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import System.Exit
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar

import System.Process

import Debug.Trace

import Lens.Micro
import Lens.Micro.Mtl

type St =  Caligraph.Cli.AppState.AppState
type KeyBindings = (Map.Map ([Modifier],Key) (Cmd St))

data CmdOutput = CmdOutput
    { cmdoutStderr :: [String]
    , cmdoutSuspendGui :: Max Bool
    }

instance Semigroup CmdOutput where
    (<>) (CmdOutput a1 a2) (CmdOutput b1 b2) =
        CmdOutput (mappend a1 b1) (mappend a2 b2)

instance Monoid CmdOutput where
    mempty = CmdOutput mempty mempty

requestSuspendGui = lift breakMonad

--bindsInternal :: Map.Map ([Modifier],Key) (Cmd St)
--bindsInternal = Map.fromList
--  [ (([], KEsc), quit_cmd)
--  , (([], KChar 'q'), quit_cmd)
--  , (([], KChar 'e'), edit_externally_cmd)
--  , (([], KChar 'a'), add_reminder_cmd)
--  , (([], KChar 'o'), dayGrid %= DayGrid.gotoToday)
--
--  , (([MCtrl], KChar 'd'), dayGrid %= DayGrid.scrollPage 0.45)
--  , (([MCtrl], KChar 'u'), dayGrid %= DayGrid.scrollPage (-0.45))
--  , (([MCtrl], KChar 'f'), dayGrid %= DayGrid.scrollPage 0.90)
--  , (([MCtrl], KChar 'b'), dayGrid %= DayGrid.scrollPage (-0.90))
--  , (([], KChar '['), focus_month_relative_cmd (-1))
--  , (([], KChar ']'), focus_month_relative_cmd 1)
--  , (([], KChar '{'), focus_month_relative_cmd (-12))
--  , (([], KChar '}'), focus_month_relative_cmd 12)
--  -- , (([], KChar '$'), shell_cmd "~/.reminders.d/bla.sh")
--  , (([], KChar '$'), shell_cmd "~/.reminders.d/mkpdf.sh")
--  , (([], KChar 'X'), toggle_log_cmd)
--
--  -- hjkl
--  , (([], KChar 'h'), focus_cmd DirLeft)
--  , (([], KChar 'j'), focus_cmd DirDown)
--  , (([], KChar 'k'), focus_cmd DirUp)
--  , (([], KChar 'l'), focus_cmd DirRight)
--  -- arrow keys
--  , (([], KLeft ), focus_cmd DirLeft)
--  , (([], KDown ), focus_cmd DirDown)
--  , (([], KUp   ), focus_cmd DirUp)
--  , (([], KRight), focus_cmd DirRight)
--  ]

commands :: Map.Map String (CCommand.CommandArgParser (Cmd St))
commands = Map.fromList
  [ (,) "quit" $ return quit_cmd
  , (,) "edit-src" $ return edit_externally_cmd
  , (,) "add" $ return add_reminder_cmd
  , (,) "goto-today" $ return (dayGrid %= DayGrid.gotoToday)
  , (,) "scroll-page" $
    return (\d -> dayGrid %= DayGrid.scrollPage d) <*> a
  , (,) "focus-month" $
    return focus_month_relative_cmd <*> a
  , (,) "shell" $ return shell_cmd <*> a
  , (,) "toggle-log" $ return toggle_log_cmd
  , (,) "focus" $ return focus_cmd <*> a
  , (,) "cycle-calendar" $ return cycle_calendar_focus_cmd
  ]
  where
    a :: CfgTypes.UserReadShow x => CCommand.CommandArgParser x
    a = CCommand.arg

quit_cmd :: Cmd St
quit_cmd =
    aboutToQuit .= True

log_message :: MonadIO m => String -> StateT St m ()
log_message msg = do
    now <- liftIO getCurrentTime
    messages %= (:) (now,msg)

toggle_log_cmd :: Cmd St
toggle_log_cmd =
    showLogLines %= (*) (-1)

focus_month_relative_cmd :: Integer -> Cmd St
focus_month_relative_cmd diff = do
    dayGrid %= DayGrid.modifyFocus (addGregorianMonthsClip diff)

focus_cmd :: Dir -> Cmd St
focus_cmd dir = do
    focus <- use $ dayGrid . DayGrid.focusDay
    reminders <- readOnly $ getReminders focus
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
      reminderInDir :: Int -> [CB.Incarnation a] -> Either (Maybe Int) Int
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

cycle_calendar_focus_cmd :: Cmd St
cycle_calendar_focus_cmd = do
  count <- fmap length $ use calendars
  idx <- use calendarFocused
  calendarFocused .= (idx + 1) `mod` count

-- | Execute the given line in the shell
shell_cmd :: String -> Cmd St
shell_cmd cmd = do
    let cp = CreateProcess (ShellCommand cmd) Nothing Nothing NoStream CreatePipe CreatePipe True False False True False False Nothing Nothing True
    chan <- use eventChannel
    thread_id <- liftIO $ forkIO $ do
        (_, procout, procerr, proc) <- createProcess cp
        err_finished <- newEmptyMVar
        out_finished <- newEmptyMVar
        forkIO $ consumeThenNotify procout (writeBChan chan . ProcessOutput cmd) out_finished
        forkIO $ consumeThenNotify procerr (writeBChan chan . ProcessError  cmd) err_finished
        takeMVar err_finished
        takeMVar out_finished
        exitCode <- waitForProcess proc
        writeBChan chan (ProcessFinished cmd exitCode)
    return ()
    where
        consumeThenNotify :: Maybe Handle -> (String -> IO ()) -> MVar () -> IO ()
        consumeThenNotify maybe_handle lineConsumer whenFinished = do
            case maybe_handle of
                Nothing -> return ()
                Just handle ->
                    whileM_ (fmap not $ hIsEOF handle) (hGetLine handle >>= lineConsumer)
            putMVar whenFinished ()

edit_externally_cmd :: Cmd St
edit_externally_cmd = do
    day <- use (dayGrid . DayGrid.focusDay)
    rems <- readOnly $ getReminders day
    idx <- fmap (fromMaybe $ length rems - 1) $ use focusItem
    if idx >= 0 && idx < length rems
    then do
        let (i,p) = CB.itemId $ rems !! idx
        requestSuspendGui
        forCalendar i $ CC.editExternally p
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
    idx <- use calendarFocused
    forCalendar idx (CC.addReminder pr)

runMessageWriter :: MonadIO m => (w -> LogLine) -> StateT St (WriterT [w] m) a -> StateT St m a
runMessageWriter msg prog = do
    (a, ws) <- mapStateT (fmap (\((a,s),w) -> ((a,w),s)) . runWriterT) prog
    forM_ ws (log_message . msg)
    return a


forEachCalendar :: MonadIO m => CC.CalendarT m a -> StateT St m [a]
forEachCalendar prog = do
    cals <- use calendars
    let indices = take (length cals) [0..]
    mapM (\i -> forCalendar i prog) indices

forCalendar :: MonadIO m => Int -> CC.CalendarT m a -> StateT St m a
forCalendar idx prog = do
    cals <- use calendars
    let prependCalName str = (T.unpack $ fst $ cals !! idx) ++ ": " ++ str
    runMessageWriter prependCalName (zoom (calendar_idx idx) prog)

fixFocusItem :: Monad m => StateT St m ()
fixFocusItem = do
    day <- use $ dayGrid . DayGrid.focusDay
    reminders <- readOnly $ getReminders day
    focusItem %= fmap (min $ length reminders - 1)

footer_height = 2

drawUI st =
  [vBox $ mapMaybe id
      [ Just $ DayGrid.render (st^.dayGrid)
      , logWindow
      , Just $ drawStatusLine st
      , Just $ input_line
      ]
  ]
  where
    day = st^.dayGrid^.DayGrid.focusDay
    reminders = runReader (getReminders day) st
    idx = (fromMaybe $ length reminders - 1) $ st^.focusItem
    focused_calendar_idx = if (idx >= 0 && idx < length reminders) then fst $ CB.itemId (reminders !! idx) else (-1)
    input_line = withAttr "inputline"
      $ padRight BT.Max
      $ str
      $ case (st^.mode) of
          AMAppend ->
            "-- INSERT --"
          AMNormal ->
            fromMaybe "" $ fmap snd $ listToMaybe (st^.messages)
    showLocalTime (LocalTime day (TimeOfDay h m s)) =
        let MkFixed i = s in
        printf "%s %02d:%02d:%02d.%03d" (show day) h m (i `div` (resolution s)) ((i * 1000) `div` (resolution s) `mod` 1000)
    logWindow =
      if (st^.showLogLines) < 0 then Nothing else Just $
        take (st^.showLogLines) (st^.messages)
        & map (\(t,m) ->
            withAttr ("log" <> "timestamp") (str $ showLocalTime $ utcToLocalTime (st^.timeZone) t)
            <+>  withAttr ("log" <> "message") (str (' ':m))
            )
        & reverse
        & vBox
        & (forceAttr ("log" <> "border") hBorder <=>)

drawStatusLine st = withAttr "statusline" $
    hBox (map drawCalendarName $ zip [0..] (st^.calendars))
    <+>
    (padLeft BT.Max $ (str $ range_visible_str))
  where
    range_visible_str =
      let (from,to) = DayGrid.rangeVisible $ (st^.dayGrid)
      in show from ++ " to " ++ show to
    drawCalendarName (idx,(name,cal)) =
        let
          attrForSuffix = "calendar" <> attrName (T.unpack name) <> "filled"
          attrForName =
            if idx == (st^.calendarFocused)
            then "calendar" <> attrName (T.unpack name) <> "text" <> "underline"
            else attrForSuffix
        in
        (withAttr ("calendar" <> attrName (T.unpack name) <> "separator")
          (str "▏")
          -- (str "▎")
          )
        <+>
        (withAttr attrForName $ txt name)
          <+>
        (withAttr attrForSuffix
         $ str
         $ if CC.openQueryCount cal > 0 then "+" else " ")

getReminders :: Monad m => Day -> ReaderT St m [CB.Incarnation (Int,Ptr)]
getReminders day =
    do
    cals <- asks _calendars
    incs <- forM (zip [0..] cals) (\(i,(_,c)) ->
        return $ map (fmap ((,) i))
               $ fromMaybe []
               $ safeArray (CC.cachedIncarnations c (day,day)) day)
    return $ L.sort $ concat incs

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
  App { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = (\s ev ->
            do
            dg <- DayGrid.updateWidgetSize (s^.dayGrid)
            s' <- return (s & (dayGrid .~ dg))
            myHandleEvent s' ev
        )
      , appStartEvent = (\s -> tryEnableMouse >> return s)
      , appAttrMap = (\s ->
        attrMap defAttr $
        [ ("cellBorder", fg white)
        , ("cellHeader", yellow `on` black)
        , ("cellHeaderFocus", yellow `on` black)
        , ("cellHeaderToday", black `on` yellow)
        , ("cellHeaderFocusToday", black `on` yellow)
        , ("dayOfWeek", magenta `on` black)
        , ("statusline", blue `on` black)
        , ("reminderTitle", defAttr)
        , ("reminderTime", Attr (SetTo bold) (SetTo green) KeepCurrent KeepCurrent)
        , ("selectedReminderTitle", bg black)
        , (Brick.editFocusedAttr, bg black)
        , ("selectedReminderTime", Attr (SetTo bold) (SetTo green) (SetTo black) KeepCurrent)
        , ("log" <> "border", fg black)
        , ("log" <> "timestamp", fg white)
        , ("log" <> "message", defAttr)
        ] ++
          do
            (name,cal) <- _calendars s
            let calcfg = CC.calendarConfig cal
            let attr = Attr
                   Default (setTo $ CalendarConfig.colorInv calcfg)
                   (setTo $ CalendarConfig.color calcfg) KeepCurrent
            let prefix = attrName "calendar" <> attrName (T.unpack name)
            [ (prefix <> "filled", attr),
              (prefix <> "text", attr),
              (prefix <> "text" <> "underline",
                Attr (SetTo underline) (setTo $ CalendarConfig.colorInv calcfg)
                     (setTo $ CalendarConfig.color calcfg) KeepCurrent),
              (prefix <> "text" <> "time",
                Attr (SetTo bold) (setTo $ CalendarConfig.colorInv calcfg)
                     (setTo $  CalendarConfig.color calcfg) KeepCurrent),
              (prefix <> "separator",
                Attr Default (SetTo black)
                     (setTo $ CalendarConfig.color calcfg) KeepCurrent),
              (prefix <> "text" <> "selected",
                Attr Default (setTo $ CalendarConfig.color calcfg)
                     (setTo $ CalendarConfig.colorInv calcfg) KeepCurrent),
              (prefix <> "text" <> "selected" <> "time",
                Attr (SetTo bold) (setTo $  CalendarConfig.color calcfg)
                     (setTo $ CalendarConfig.colorInv calcfg) KeepCurrent) ]
        )
      }
      where
        setTo Nothing = Default
        setTo (Just x) = SetTo x

scrollStep = 3

execCmd :: St -> Cmd St -> EventM WidgetName (Next St)
execCmd s cmd =
    let Breakpoint io_action = flip runStateT s $
            do
            cmd
            dr <- use dayRange
            new_dr <- fmap DayGrid.rangeVisible $ use dayGrid
            when (dr /= new_dr) $ do
                dayRange .= new_dr
                void $ forEachCalendar (CC.setRangeVisible new_dr)
            void $ forEachCalendar CC.fileQuery
            s <- get
            dayGrid %= (DayGrid.resizeDays $ day2widget s)
    in
    do
        possibly_io <- liftIO $ io_action
        case possibly_io of
            Pure ((),s') ->
                if (s')^.aboutToQuit
                then halt s'
                else continue s'
            Monadic fg_io_action ->
                suspendAndResume (fmap (\((), s) -> s) fg_io_action)

myHandleEvent :: St -> BrickEvent WidgetName ExternalEvent -> EventM WidgetName (Next St)
myHandleEvent s (VtyEvent e) =
  case e of
    EvKey key mods ->
      case (s^.mode) of
        AMAppend ->
          case (key,mods) of
            (KEsc,[]) ->
              execCmd s $ mode .= AMNormal
            (KEnter,[]) ->
              execCmd s $ do
                  editor <- use newReminderEditor
                  newReminderEditor .= emptyReminderEditor
                  mode .= AMNormal
                  addReminderFromString (head (getEditContents editor))
            _ ->
                handleEventLensed s newReminderEditor Brick.handleEditorEvent (EvKey key mods)
                >>= flip execCmd (return ())
        AMNormal ->
          case Map.lookup (mods,key) (s^.binds) of
            Just cmd ->
                execCmd s cmd
            Nothing -> continue s
    EvResize w h ->
      execCmd s $ dayGrid %= DayGrid.resize (w,h)
    EvMouseDown _ _ BScrollDown _ ->
      execCmd s $ dayGrid %= DayGrid.scroll scrollStep
    EvMouseDown _ _ BScrollUp _ ->
      execCmd s $ dayGrid %= DayGrid.scroll (-scrollStep)
    _ ->
      continue s
myHandleEvent s (AppEvent ev) =
    execCmd s $ do
      case ev of
        CalendarIO idx -> do
            forCalendar idx CC.receiveResult
            forCalendar idx CC.fileQuery
            c <- use (calendar_idx idx)
            s'' <- get
            dayGrid %= (DayGrid.resizeDays $ day2widget s'')
        CalendarWakeUp idx -> do
            forCalendar idx CC.receiveWakeUp
            s'' <- get
            dayGrid %= (DayGrid.resizeDays $ day2widget s'')
        ProcessFinished cmd exitCode ->
            log_message (cmd ++ " finished with " ++ show exitCode)
        ProcessOutput cmd msg ->
            log_message (cmd ++ ": " ++ msg)
        ProcessError cmd msg ->
            log_message (cmd ++ " error: " ++ msg)
        DayChanged today' -> do
            dayGrid . DayGrid.today .= today'

myHandleEvent s (MouseDown (WNDay d) BLeft _ _) =
      execCmd s $ do dayGrid %= DayGrid.setFocus d ; focusItem .= Just 0
myHandleEvent s (MouseDown (WNDayItem d idx) BLeft _ _) =
      execCmd s $ do dayGrid %= DayGrid.setFocus d ; focusItem .= Just idx
myHandleEvent s (MouseDown _ BScrollDown _ _) =
      execCmd s $ do dayGrid %= DayGrid.scroll scrollStep
myHandleEvent s (MouseDown _ BScrollUp _ _) =
      execCmd s $ do dayGrid %= DayGrid.scroll (-scrollStep)
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
                _ -> Nothing)
            (st^.calendars))
    where
      today = st^.dayGrid^.DayGrid.today
      focus = st^.dayGrid^.DayGrid.focusDay
      reminders = flip runReader st $ do
        items <- getReminders day
        return $ do
          inc <- items
          let calName = T.unpack $ fst $ (st^.calendars) !! (fst (CB.itemId inc))
          let attr = attrName "calendar" <> attrName calName <> attrName "text"
          return (CalItemStyle attr, inc)

emptyReminderEditor :: Brick.Editor String WidgetName
emptyReminderEditor =
    (Brick.editor WNNewReminder (Just 1) "")

loadKeyConfig :: T.Text -> IO KeyBindings
loadKeyConfig src = do
  kc <- rightOrDie $ fmap MainConfig.globalKeys $ MainConfig.parseKeyConfig src
  binds <- rightOrDie $ fmap Map.fromList $ forM kc (\(k,v) ->
    mapLeft (\s -> "In binding of \"" ++ userShow k ++ "\" to \"" ++ show v ++ ": " ++ s) $
    do
        cmd <- CCommand.bindFromMap (flip Map.lookup commands) v
        return (CfgTypes.keyCombi k,cmd))
  return binds

-- | given the current day, run the specific action whenever everytime day changes
reportDayChangeThread :: (Day -> IO ()) -> Day -> IO ()
reportDayChangeThread action today = do
    threadDelay (5 * 1000 * 1000) -- wait for 5 seconds
    today' <- DayGrid.getToday -- get the new day
    when (today /= today') (action today')
    reportDayChangeThread action today' -- repeat it


mainFromConfig :: [Rules.Rule] -> KeyBindings -> [(T.Text,CC.ConfiguredCalendar)] -> IO ()
mainFromConfig rules keyconfig cals =
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  in do
  -- main channel
  chan <- newBChan (10 + 2 * length cals)
  -- subscribe to day changes
  today <- DayGrid.getToday
  tz <- getCurrentTimeZone
  forkIO $ reportDayChangeThread (writeBChan chan . DayChanged) today
  -- initialize calendars
  cals_loaded <- forM (zip [0..] cals) (\(i,(t,raw_c)) -> do
    cal' <- raw_c (writeBChan chan (CalendarIO i)) (writeBChan chan (CalendarWakeUp i))
    return (t,cal'))
  -- init widgets
  let day_grid = (DayGrid.init WNDayGrid today)
  let day_range = DayGrid.rangeVisible day_grid
  -- init main state
  let initial_state = AppState False day_grid day_range (Just 0) cals_loaded
                        0 [] AMNormal emptyReminderEditor chan tz (-20)
                        keyconfig rules

  bootup_state <- flip execStateT initial_state $
    forEachCalendar (CC.setRangeVisible day_range >> CC.fileQuery)
  vty <- buildVty
  customMain vty buildVty (Just chan) mainApp bootup_state
  return ()

rightOrDie :: Either String a -> IO a
rightOrDie = either die return

