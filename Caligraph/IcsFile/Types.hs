{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Caligraph.IcsFile.Types where

import Data.Time.Clock (UTCTime)

-- | a param is: name = value[, …]
type Param = (String, String, [String])
type EncodedValue = String
type ContentLine = (String, [Param], EncodedValue)

data Tree annotation = Tree annotation String [TreeEntry annotation]
    deriving (Eq, Ord, Show, Functor)
data TreeEntry annotation
  = TeAttribute annotation ContentLine
  -- ^ an attribute
  | TeSubtree (Tree annotation)
  -- ^ another tree
  deriving (Eq, Ord, Show, Functor)


data IcsType icstype
    = ItString (icstype String)
    | ItBool (icstype Bool)
    | ItDateTime (icstype UTCTime)

mapIcsType :: (forall a. f a -> (f' a)) -> IcsType f -> (IcsType f')
mapIcsType f (ItString v) = ItString $ f v
mapIcsType f (ItBool v) = ItBool $ f v
mapIcsType f (ItDateTime v) = ItDateTime $ f v
