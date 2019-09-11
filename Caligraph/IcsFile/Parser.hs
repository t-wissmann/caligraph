module Caligraph.IcsFile.Parser where

import Caligraph.IcsFile.Types

import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec as P

foo = "bar"

type IcsLine = (Int,String)

parseLines :: GenParser Char st [(SourcePos,String)]
parseLines = flip sepEndBy (many1 $ char '\n') $ do
    c <- noneOf " "
    pos <- getPosition
    s <- many1 (noneOf "\n")
    indentLines <- many $ do
      try (char '\n' >> char ' ')
      s <- many (noneOf "\n")
      return s
    return $ (pos, (c:s) ++ concat indentLines)


parse
  :: String
  -- ^ the filepath/title for error messages
  -> String
  -- ^ the file's content
  -> Either ParseError [(SourcePos,String)]
parse fp =
  P.parse parseLines fp . filter ((/=) '\r')
