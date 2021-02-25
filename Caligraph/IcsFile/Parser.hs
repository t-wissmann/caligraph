{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Caligraph.IcsFile.Parser where

import Caligraph.IcsFile.Types

import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec as P

import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Control.Arrow (second)
import Control.Monad (forM)
import Data.Char
import Data.Either
import Data.Functor.Const
import Control.Monad.Except
import qualified Control.Monad.State as State

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
                  <*> (char '=' >> parseParamValue)
                  <*> many (char ',' >> parseParamValue)

-- | parse a param-value
--
-- Currently, we ignore case-insensitivity. The RFC5545 in sec 3.2 says:
-- "Property parameter values that are not in quoted-strings are
-- case-insensitive."
-- https://tools.ietf.org/html/rfc5545#section-3.2
parseParamValue :: GenParser Char st String
parseParamValue =
    (char '"' >> many qsafeChar <* char '"') -- quoted-string
    <|> many safeChar -- paramtext
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


-- | a value after the last syntactical ':'
parseValue :: GenParser Char st String
parseValue = many anyToken -- VALUE-CHAR: "Any textual character"

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

-- | group by begin..end-blocks
treeBuilder :: GenParser (SourcePos,ContentLine) st (Tree SourcePos)
treeBuilder = do
    parseTree
    where
        parseTree :: GenParser (SourcePos,ContentLine) st (Tree SourcePos)
        parseTree = do
            pos <- getPosition
            (_,param, groupname) <- cl_with ((==) "BEGIN")
            entries <- many parseTreeEntry
            (_,param', groupname') <- cl_with  ((==) "END")
            return $ Tree pos groupname entries

        parseTreeEntry :: GenParser (SourcePos,ContentLine) st (TreeEntry SourcePos)
        parseTreeEntry =
            try (TeSubtree <$> parseTree)
            <|>
            do
                pos <- getPosition
                cl <- cl_with ((/=) "END")
                return $ TeAttribute pos cl

        contentline :: GenParser (SourcePos,ContentLine) st ContentLine
        contentline = token (show . snd) fst (Just . snd)

        cl_with :: (String -> Bool) -> GenParser (SourcePos,ContentLine) st ContentLine
        cl_with pred = token (show . snd) fst (\(_,line) ->
            let (s,_,_) = line in
            if pred (map toUpper s) then Just line else Nothing)

encodedChar :: GenParser Char st Char
encodedChar = do
    noneOf "\\" <|> do
        char '\\'
        c <- anyToken
        return $ case c of
                    'N' -> '\n'
                    'n' -> '\n'
                    c   -> c

-- | parse the entire input into a string
encodedString :: GenParser Char st String
encodedString = do
    s <- many encodedChar
    eof
    return s

tryRead :: Read a => String -> GenParser Char st a
tryRead str =
    case reads str of
    ((v, suffix):_) ->
        if all isSpace suffix
        then return v
        else fail $ "Could not parse: " ++ suffix
    [] -> fail $ "Could not parse: " ++ str


data ConvertTo a b = ConvertTo { convertTo :: (b -> a) }

typedValueParser :: (IcsType (ConvertTo typ)) -> GenParser Char st typ
typedValueParser (ItString ret) = convertTo ret <$> encodedString
typedValueParser (ItBool ret) = convertTo ret <$> do
    s <- many anyToken
    case map toUpper s of
        "TRUE" -> return True
        "FALSE" -> return False
        _ -> fail $ "Not a boolean value: " ++ s
typedValueParser (ItDateTime ret) = convertTo ret <$> do
    y <- count 4 digit
    m <- count 2 digit
    d <- count 2 digit
    day <- tryRead (y ++ "-" ++ m ++ "-" ++ d)
    char 'T'
    hh <- count 2 digit >>= tryRead
    mm <- count 2 digit >>= tryRead
    ss <- count 2 digit >>= tryRead
    let time = (((hh * 60) + mm) * 60 + ss) :: Integer
    eof
    return $ UTCTime day (secondsToDiffTime time)

decodeValue :: (forall f. f t -> IcsType f) -> EncodedValue -> Either ParseError t
decodeValue icstype src = P.parse (typedValueParser $ icstype $ ConvertTo id) src src

parse
  :: String
  -- ^ the filepath/title for error messages
  -> String
  -- ^ the file's content
  -> Either ParseError (Tree SourcePos)
parse fp filecontent = do
  -- unfold wrapped lines
  unfolded <- unfoldLines fp filecontent
  -- tokenize each of these lines
  contlines <- forM unfolded $ \(pos, line) -> do
                  (,) pos <$> P.parse parseContentLine (fp ++ show pos) line
  -- turn them into a tree
  P.parse treeBuilder fp contlines

parseFile
  :: String
  -- ^ the filepath
  -> IO (Either ParseError (Tree SourcePos))
parseFile fp = do
  content <- readFile fp
  return $ parse fp content
