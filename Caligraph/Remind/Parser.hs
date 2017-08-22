{-# LANGUAGE StrictData #-}

module Caligraph.Remind.Parser where

import Caligraph.Remind.Types

import Prelude hiding (rem)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec as P
import Data.Maybe (catMaybes)

int :: GenParser Char st Int
int = read <$> many1 digit


rfLine :: GenParser Char st RFLine
rfLine =
      pref "REM " Rem rem
  <|> pref "FSET " Fset fset
  <|> pref "#"    Comment (many (noneOf "\n"))
  where
    pref :: String -> (a -> RFLine) -> GenParser Char st a -> GenParser Char st RFLine
    pref prefix constructor parser = do
            spaces
            string prefix
            x <- parser
            return (constructor x)

month :: GenParser Char st Int
month =
  pmonth
  where
    month_names =
      [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
      , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
      ]
    pmonth =
      foldl1 (<|>) $
      map (\(idx,name) -> (try (string name) >> return idx)) $
      zip [1..(1+length month_names)] month_names

dateUS :: GenParser Char st PartialDate
dateUS = do
  m <- optionMaybe month
  many1 space
  y <- optionMaybe int
  many1 space
  d <- optionMaybe int
  return $ PartialDate {
    pday = d,
    pmonth = m,
    pyear = y
  }

dateISO :: GenParser Char st PartialDate
dateISO = do
  y <- int
  char '-'
  m <- int
  char '-'
  d <- int
  return $ PartialDate {
    pday = Just d,
    pmonth = Just m,
    pyear = Just y
  }

date :: GenParser Char st PartialDate
date = do
  spaces
  (try dateISO <|> try dateUS)

offset :: GenParser Char st Int
offset = do
  spaces
  (char '+' >> int) <|> (char '-' >> (fmap (* (-1)) int))

time :: GenParser Char st Time
time = do
  h <- int
  char ':'
  m <- int
  return $ Time h m

msg :: GenParser Char st String
msg = do
  spaces
  string "MSG "
  many1 (noneOf "\n")

rem :: GenParser Char st REM
rem = do
  spaces
  args <- many (do x <- try remArg ; many1 space ; return x)
  m <- msg
  eof
  return (REM args m)

remArg :: GenParser Char st RemArg
remArg = do
  (fmap Date_spec $ try date)
  <|> (fmap Delta $ try offset)
  <|> (fmap Repeat $ try (char '*') >> int)
  <|> (fmap AT $ try (string "AT" >> many1 space) >> time)
  <|> (fmap DURATION $ try (string "DURATION" >> many1 space) >> time)
  <|> (fmap UNTIL $ try (string "UNTIL" >> many1 space) >> date)


fset :: GenParser Char st String
fset = many1 (noneOf "\n")

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

groupLines :: GenParser Char st [(Int, String)]
groupLines =
  fmap joinRawLines parseRawLines
  where
    parseSingleRawLine :: GenParser Char st (Int,String)
    parseSingleRawLine = do
      spaces
      pos <- getPosition
      str <- many1 $ noneOf ("\n")
      return (sourceLine pos, str)

    parseRawLines :: GenParser Char st [(Int,String)]
    parseRawLines = do
      lines <- parseSingleRawLine `sepEndBy` (oneOf "\r\n")
      eof
      return lines

    joinRawLines :: [(Int,String)] -> [(Int,String)]
    joinRawLines [] = []
    joinRawLines [ls] = [ls]
    joinRawLines ((l1,str1):tl) =
      case (reverse str1, joinRawLines (tl)) of
        (('\\':str1rev), (l2,str2):tl') ->
          -- join the lines
          (l1, reverse str1rev ++ str2) : tl'

        (('\\':str1rev), []) ->
          [(l1,reverse str1rev)]

        (_, res) -> (l1,str1) : res


parse :: String -> IO [Either ParseError (Int,RFLine)]
parse path = do
  input <- readFile path
  let lines = P.parse groupLines path input
  return $ either (\x -> [Left x]) (map parseNumberedLine) lines
  where
    mapSnd f (a,b) = (a,f b)
    offsetLine idx =  do
        pos <- getPosition
        setPosition (setSourceLine pos idx)
        rfLine
    parseNumberedLine (idx, str) =
      case P.parse (offsetLine idx) path str of
        Right e -> Right (idx,e)
        Left e -> Left e



