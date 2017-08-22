{-# LANGUAGE StrictData #-}

module Caligraph.Remind.Parser where

data Line a =
    Comment a String
  | Include a String
  | Rem a String
  | Omit a String
  | Fset a String
  deriving (Eq,Show)


-- parse : String -> IO [Line ()]
parse str = return [Comment () str]


