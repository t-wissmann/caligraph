{-# LANGUAGE StrictData #-}

module Caligraph.Remind.Types where

data RFLine =
    Comment String
  | Include String
  | Rem REM
  | Omit String
  | Fset String
  deriving (Eq,Show)

data REM = REM [RemArg] String deriving (Eq,Show)

data RemArg =
    ONCE
  | Date_spec PartialDate
  | Delta Int
  | Repeat Int
  | AT Time
  | DURATION Time
  | UNTIL PartialDate
  deriving (Eq,Show)

data Time = Time
  { thour :: Int
  , tmin :: Int
  } deriving (Eq,Show)

data PartialDate = PartialDate
  { pday  :: Maybe Int
  , pmonth :: Maybe Int
  , pyear :: Maybe Int
  } deriving (Eq,Show)


