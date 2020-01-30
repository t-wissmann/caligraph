module Caligraph.IcsFile.Parser where

import Caligraph.IcsFile.Types

import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec as P

import Data.Char
import Data.Either

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
type Param = (String, String, [String])
type ContentLine = (String, [Param], String)

-- | parse param-name, here we do not distinguish between iana-token
-- and x-name
parseName :: GenParser Char st String
parseName = many1 $ oneOf $ ['0'..'9'] ++ "-" ++ ['a'..'z'] ++ ['A'..'Z']

-- | parse a param
-- param         = param-name "=" param-value *("," param-value)
parseParam :: GenParser Char st Param
parseParam = (,,) <$> parseName
                  <*> parseValue
                  <*> (parseValue  `sepBy` (char ','))

parseValue :: GenParser Char st String
parseValue = (char '"' >> many qsafeChar <* char '"')
    <|> many safeChar
    where
        -- QSAFE-CHAR    = WSP / %x21 / %x23-7E / NON-US-ASCII
        -- ; Any character except CONTROL and DQUOTE
        qsafeChar :: GenParser Char st Char
        qsafeChar = noneOf (control ++ "\"")
        --  SAFE-CHAR     = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E / NON-US-ASCII
        -- ; Any character except CONTROL, DQUOTE, ";", ":", ","
        safeChar :: GenParser Char st Char
        safeChar = noneOf (control ++ "\";:,")

        -- CONTROL       = %x00-08 / %x0A-1F / %x7F
        -- ; All the controls except HTAB
        control :: [Char]
        control = map chr $ [0x00..0x08] ++ [0x0A..0x1F] ++ [0x7F]


-- | parse content lines in the sense of section 3.1 of RFC5545
parseContentLine :: GenParser Char st ContentLine
parseContentLine =
   (,,) <$> parseName
        <*> many (char ';' >> parseParam)
        <*> (char ':' >> parseValue <* char '\n')

parse
  :: String
  -- ^ the filepath/title for error messages
  -> String
  -- ^ the file's content
  -> ([ParseError], [(SourcePos,ContentLine)])
parse fp filecontent = partitionEithers $
  case (P.parse parseLines fp $ filter ((/=) '\r') filecontent) of
    Left parseError -> [Left parseError]
    Right lines ->
        map (\(sp,str) -> (,) sp <$> P.parse parseContentLine (fp ++ show sp) str) lines
