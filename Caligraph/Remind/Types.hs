{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}



module Caligraph.Remind.Types where

import Data.Functor.Classes
import Control.Monad.Identity
import Data.Functor.Contravariant (Op(Op,getOp))
import Data.Maybe
import Data.Time.Calendar (Day,fromGregorianValid)

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
  | DateSpec (f PartialDate)
  | Delta (f Int)
  | Repeat (f Int)
  | AT (f (Int,Int))
  | DURATION (f (Int,Int))
  | UNTIL (f Day)

deriving instance Eq (RemArgT Identity)
deriving instance Show (RemArgT Identity)

type RemArg = RemArgT Identity

-- | Extract the argument of a RemArg constructor
coApp :: RemArgT (Op a) -> RemArgT Identity -> Maybe a
coApp q val = case (q,val) of
    (ONCE f,        ONCE x)         -> j f x
    (DateSpec f,    DateSpec x)    -> j f x
    (Delta f,       Delta x)        -> j f x
    (Repeat f,      Repeat x)       -> j f x
    (AT f,          AT x)           -> j f x
    (DURATION f,    DURATION x)     -> j f x
    (UNTIL f,       UNTIL x)        -> j f x
    (_, _) -> Nothing
    where j f y = Just $ getOp f $ runIdentity y

getRemArg
    :: (Op a a -> RemArgT (Op res))
    -- ^ some constructor of RemArgT
    -> RemArg -> Maybe res
getRemArg constructor = coApp (constructor $ Op id)

findRemArg
    :: (Op a a -> RemArgT (Op res))
    -- ^ some constructor of RemArgT
    -> [RemArg]
    -- ^ a list of arguments
    -> Maybe res
    -- ^ the parameter to the first element with the matching constructor
findRemArg constr = listToMaybe . mapMaybe (getRemArg constr)

data PartialDate = PartialDate
  { pday  :: Maybe Int
  , pmonth :: Maybe Int
  , pyear :: Maybe Integer
  } deriving (Eq,Show)

flatPartialDate :: Maybe PartialDate -> PartialDate
flatPartialDate = maybe (PartialDate Nothing Nothing Nothing) id

isFullDate :: PartialDate -> Maybe Day
isFullDate (PartialDate (Just d) (Just m) (Just y)) =
    fromGregorianValid y m d
isFullDate _ = Nothing


