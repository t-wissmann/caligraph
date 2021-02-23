module Caligraph.IcsFile.Types where

-- | a param is: name = value[, …]
type Param = (String, String, [String])
type ContentLine = (String, [Param], String)

data Tree annotation = Tree annotation String [TreeEntry annotation]
    deriving (Eq, Ord, Show)
data TreeEntry annotation
  = TeAttribute annotation ContentLine
  -- ^ an attribute
  | TeSubtree annotation (Tree annotation)
  -- ^ another tree
  deriving (Eq, Ord, Show)


