{-# LANGUAGE TemplateHaskell #-}
module Caligraph.CalDav.Backend where

import qualified Caligraph.Backend.Types as CB
import qualified Caligraph.Backend.Utils as CB

import Data.Time.Calendar
import qualified Data.Array as A

type St = ()
type Event = ()


parseConfig :: (String -> Maybe String) -> Either String (St, CB.WakeUpLoop Event)
parseConfig cfg =
  return ((), (\cb -> return ()))

cachedIncarnations :: St -> (Day,Day) -> CB.Incarnations'
cachedIncarnations st (from,to) =
  A.array (from,to) [(d, []) | d <- [from..to]]

handleEvent :: CB.Event Event -> CB.BackendM St Event ()
handleEvent (CB.SetRangeVisible (from,to)) = return ()
handleEvent (CB.AddReminder pr) = return ()
handleEvent (CB.Response ()) = return ()

backend :: CB.Backend St Event
backend = CB.Backend
  { CB.create = parseConfig
  , CB.cachedIncarnations = cachedIncarnations
  , CB.itemSource = (\ptr -> do
      return $ CB.ExistingFile ("/dev/null", 0) ()
      )
  , CB.handleEvent = handleEvent
  }

