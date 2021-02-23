module Caligraph.IcsFile.Parser where

import Caligraph.IcsFile.Types

import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec as P

import Control.Arrow (second)
import Control.Monad (forM)
import Data.Char
import Data.Either

type IcsLine = (Int,String)

parseLines :: GenParser Char st [(SourcePos,String)]
parseLines = flip sepEndBy (many1 $ char '\n') $ do
    pos <- getPosition
    c <- noneOf " "
    s <- many (noneOf "\n")
    indentLines <- many $ do
      try (char '\n' >> oneOf " \t")
      s <- many (noneOf "\n")
      return s
    return $ (pos, (c:s) ++ concat indentLines)

-- | parse param-name, here we do not distinguish between iana-token
-- and x-name
parseName :: GenParser Char st String
parseName = many1 $ oneOf $ ['0'..'9'] ++ "-" ++ ['a'..'z'] ++ ['A'..'Z']

-- | parse a param
-- param         = param-name "=" param-value *("," param-value)
parseParam :: GenParser Char st Param
parseParam = (,,) <$> parseName
                  <*> (char '=' >> parseValue)
                  <*> many (char ',' >> parseValue)

-- | parse a param-value
--
-- Currently, we ignore case-insensitivity. The RFC5545 in sec 3.2 says:
-- "Property parameter values that are not in quoted-strings are
-- case-insensitive."
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
        <*> (char ':' >> parseValue)

unfoldLines
  :: String
  -- ^ the filepath/title for error messages
  -> String
  -- ^ the file's content
  -> Either ParseError [(SourcePos,String)]
unfoldLines fp =
    P.parse parseLines fp . filter ((/=) '\r')

-- | group all BEGIN...END-Blocks to trees
buildTree :: [TreeEntry annotation] -> Either ParseError [TreeEntry annotation]
buildTree = Right -- TODO

parse
  :: String
  -- ^ the filepath/title for error messages
  -> String
  -- ^ the file's content
  -> Either ParseError [TreeEntry SourcePos]
parse fp filecontent = do
  -- unfold wrapped lines
  unfolded <- unfoldLines fp filecontent
  -- tokenize each of these lines
  contlines <- forM unfolded $ \(pos, line) -> do
                  TeAttribute pos <$> P.parse parseContentLine (fp ++ show pos) line
  -- turn them into a tree
  buildTree contlines
