
module Caligraph.Cli.UnicodeJunction where

import Data.List (find)
data LineType =
    Empty
  | Normal
  | Strong
  deriving (Eq,Show,Enum)

char2lineType ch =
  case ch of
    ' ' -> Empty
    '-' -> Normal
    '_' -> Strong
    v -> error ("Unknown LineType character \'" ++ [v] ++ "\'")

-- to be characterised:
-- ┄┅┆┇┈┉┊┋╌╍╎╏═║╒╓╔╕
-- ╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧
-- ╨╩╪╫╬╭╮╯╰

raw_characters =
  -- seach of the following strings consists of
  --  - A unicode junction character
  --  - the line type to the north cell
  --  - the line type to the east cell
  --  - the line type to the south cell
  --  - the line type to the west cell
  -- 
  [ "─ - -" , "━ _ _" , "│- - " , "┃_ _ "
  , "┌ -- " , "┍ _- " , "┎ -_ " , "┏ __ "
  , "┐  --" , "┑  -_" , "┒  _-" , "┓  __"
  , "└--  " , "┕-_  " , "┖_-  " , "┗__  "
  , "┘-  -" , "┙-  _" , "┚_  -" , "┛_  _"
  , "├--- " , "┝-_- " , "┞__- " , "┟--_ "
  , "┠_-_ " , "┡__- " , "┢-__ " , "┣___ "
  , "┤- --" , "┥- -_" , "┦_ --" , "┧- _-"
  , "┨_ _-" , "┩_ -_" , "┪- __" , "┫_ __"
  , "┬ ---" , "┭ --_" , "┮ _--" , "┯ _-_"
  , "┰ -_-" , "┱ -__" , "┲ __-" , "┳ ___"
  , "┴-- -" , "┵-- _" , "┶-_ -" , "┷-_ _"
  , "┸_- -" , "┹_- _" , "┺__ -" , "┻__ _"
  , "┼----" , "┽---_" , "┾-_--" , "┿-_-_"
  , "╀_---" , "╁--_-" , "╂_-_-" , "╃_--_"
  , "╄__--" , "╅--__" , "╆-__-" , "╇__-_"
  , "╈-___" , "╉_-__" , "╊___-" , "╋____"
  , "╴   -" , "╵-   " , "╶ -  " , "╷  - "
  , "╸   _" , "╹_   " , "╺ _  " , "╻  _ "
  , "╼ _ -" , "╽- _ " , "╾ - _" , "╿_ - "
  ]

characters :: [((LineType,LineType,LineType,LineType),Char)]
characters =
  map parseLine raw_characters
  where
    parseLine (ch:n:e:s:w:[]) =
      ( ( char2lineType n
        , char2lineType e
        , char2lineType s
        , char2lineType w
        )
      , ch
      )
    parseLine l = error $ "Wrong LineType string \'" ++ l ++ "\'"

-- | Given the connections at the four neighbours
-- return the corresponding unicode border/box character
get :: LineType
    -- ^ Connection to the north
    -> LineType
    -- ^ Connection to the east
    -> LineType
    -- ^ Connection to the south
    -> LineType
    -- ^ Connection to the west
    -> Char
get n e s w = maybe '?' id $ fmap snd $ find (\(lt4,ch) -> lt4 == (n,e,s,w)) characters
