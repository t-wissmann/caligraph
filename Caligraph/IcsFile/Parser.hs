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

-- | a param is: name = value[, …]
type Param = (String, [String])
type ContentLine = (String, [Param], String)

parseName :: GenParser Char st String
parseName = many1 $ oneOf $ ['0'..'9'] ++ "-" ++ ['a'..'z'] ++ ['A'..'Z']

parseParam :: GenParser Char st Param
parseParam = error "sdf"

parseValue :: GenParser Char st String
parseValue = error "not yet defined"


parseContentLine :: GenParser Char st ContentLine
parseContentLine =
   (,,) <$> parseName
        <*> many (char ';' >> parseParam)
        <*> (char ':' >> parseValue <* many (char '\r') <* char '\n')

parse
  :: String
  -- ^ the filepath/title for error messages
  -> String
  -- ^ the file's content
  -> Either ParseError [(SourcePos,String)]
parse fp =
  P.parse parseLines fp . filter ((/=) '\r')
