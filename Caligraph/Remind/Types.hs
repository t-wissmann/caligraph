{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}



module Caligraph.Remind.Types where

import Data.Functor.Classes
import Control.Monad.Identity
import Data.Functor.Contravariant (Op(getOp))

data RFLine =
    Comment String
  | Include String
  | Rem REM
  | Omit String
  | Fset String
  deriving (Eq,Show)

data REM = REM [RemArg] String deriving (Eq,Show)

data RemArgT f =
    ONCE (f ())
  | Date_spec (f PartialDate)
  | Delta (f Int)
  | Repeat (f Int)
  | AT (f Time)
  | DURATION (f Time)
  | UNTIL (f PartialDate)

deriving instance Eq (RemArgT Identity)
deriving instance Show (RemArgT Identity)

type RemArg = RemArgT Identity

-- | Extract the argument of a RemArg constructor
coApp :: RemArgT (Op a) -> RemArgT Identity -> Maybe a
coApp q val = case (q,val) of
    (ONCE f,        ONCE x)         -> j f x
    (Date_spec f,   Date_spec x)    -> j f x
    (Delta f,       Delta x)        -> j f x
    (Repeat f,      Repeat x)       -> j f x
    (AT f,          AT x)           -> j f x
    (DURATION f,    DURATION x)     -> j f x
    (UNTIL f,       UNTIL x)        -> j f x
    (_, _) -> Nothing
    where j f y = Just $ getOp f $ runIdentity y

-- getRemArg :: (f () -> RemArgT f) ->

data Time = Time
  { thour :: Int
  , tmin :: Int
  } deriving (Eq,Show)

data PartialDate = PartialDate
  { pday  :: Maybe Int
  , pmonth :: Maybe Int
  , pyear :: Maybe Int
  } deriving (Eq,Show)


