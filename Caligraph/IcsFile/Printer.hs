module Caligraph.IcsFile.Printer where

import Caligraph.IcsFile.Types
import Data.List (intersperse)

prettyShowTree :: Show annotation => Tree annotation -> String
prettyShowTree = concat . intersperse "\n" . treeLines
    where
    treeEntryLines :: Show annotation => TreeEntry annotation -> [String]
    treeEntryLines (TeAttribute a line) = [show line ++ "   " ++ show a]
    treeEntryLines (TeSubtree tree) = treeLines tree

    treeLines :: Show annotation => Tree annotation -> [String]
    treeLines (Tree a group entries) =
        (["BEGIN:" ++ group ++ "   " ++ show a] :: [String])
        ++ ((do
                entry <- entries
                line <- (treeEntryLines entry :: [String])
                return (' ' : line)
           ) :: [String])
        ++ ["END:" ++ group]

prettyPrintTree :: Show annotation => Tree annotation -> IO ()
prettyPrintTree tree = putStrLn $ prettyShowTree tree
