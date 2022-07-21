{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Caligraph.IcsFile.Types where

import Data.Time.Clock (UTCTime)

import qualified Data.HashMap.Strict as Map

-- | a param is: name = value[, …]
type Param = (String, String, [String])
type EncodedValue = String
type ContentLine = (String, [Param], EncodedValue)

data TreeType = VEVENT | TreeTypeOther String deriving (Eq, Ord)

instance (Show TreeType) where
    show VEVENT = "VEVENT"
    show (TreeTypeOther s) = s

instance (Read TreeType) where
    readsPrec _ s
        | s == "VEVENT"     = just VEVENT
        | otherwise         = just $ TreeTypeOther s
        where just x = [(x, "")]

data Tree annotation = Tree
    { treeAnnotation :: annotation
    , treeType :: TreeType
    , treeChildren :: [TreeEntry annotation]
    , treeEntryMap :: Map.HashMap String ContentLine
    }
    deriving (Eq, Ord, Show, Functor)

data TreeEntry annotation
  = TeAttribute annotation ContentLine
  -- ^ an attribute
  | TeSubtree (Tree annotation)
  -- ^ another tree
  deriving (Eq, Ord, Show, Functor)

data CompiledEvent = CompiledEvent
    { ceStart :: UTCTime
    , ceSummary :: String
    }

data IcsType icstype
    = ItString (icstype String)
    | ItBool (icstype Bool)
    | ItDateTime (icstype UTCTime)

mapIcsType :: (forall a. f a -> (f' a)) -> IcsType f -> (IcsType f')
mapIcsType f (ItString v) = ItString $ f v
mapIcsType f (ItBool v) = ItBool $ f v
mapIcsType f (ItDateTime v) = ItDateTime $ f v
