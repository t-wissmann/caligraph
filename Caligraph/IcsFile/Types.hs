module Caligraph.IcsFile.Types where

data Tree = Tree String [TreeEntry]
data TreeEntry
  = TeAttribute String String
  -- ^ an attribute Key:Value
  | TeSubtree Tree
  -- ^ another tree


