module Caligraph.IcsFile.Types where

-- | a param is: name = value[, …]
type Param = (String, String, [String])
type ContentLine = (String, [Param], String)

data Tree = Tree String [TreeEntry]
data TreeEntry
  = TeAttribute ContentLine
  -- ^ an attribute
  | TeSubtree Tree
  -- ^ another tree


