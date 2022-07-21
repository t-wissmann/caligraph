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
    treeLines tree =
        (["BEGIN:" ++ show (treeType tree) ++ "   " ++ show (treeAnnotation tree)] :: [String])
        ++ ((do
                entry <- treeChildren tree
                line <- (treeEntryLines entry :: [String])
                return (' ' : line)
           ) :: [String])
        ++ ["END:" ++ show (treeType tree)]

prettyPrintTree :: Show annotation => Tree annotation -> IO ()
prettyPrintTree tree = putStrLn $ prettyShowTree tree
