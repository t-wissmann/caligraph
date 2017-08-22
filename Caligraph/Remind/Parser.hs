{-# LANGUAGE StrictData #-}

module Caligraph.Remind.Parser where

import Prelude hiding (rem)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec as P

type RemFile = [RFLine]

data RFLine =
    Comment String
  | Include String
  | Rem REM
  | Omit String
  | Fset String
  deriving (Eq,Show)

data PartialDate = PartialDate
  { pday  :: Maybe Int
  , pmonth :: Maybe Int
  , pyear :: Maybe Int
  } deriving (Eq,Show)

remFile :: GenParser Char st RemFile
remFile = do
  many eol
  x <- rfLine `endBy` (many eol)
  eof
  return x

int :: GenParser Char st Int
int = spaces >> (read <$> many1 digit)

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

rfLine :: GenParser Char st RFLine
rfLine =
      pref "REM " Rem rem
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
  spaces >> pmonth
  where
    month_names =
      [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
      , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
      ]
    pmonth =
      foldl1 (<|>) $
      map (\(idx,name) -> (try (string name) >> return idx)) $
      zip [1..(1+length month_names)] month_names
date :: GenParser Char st PartialDate
date = do
  spaces
  m <- optionMaybe month
  y <- optionMaybe int
  d <- optionMaybe int
  return $ PartialDate {
    pday = d,
    pmonth = m,
    pyear = y
  }

offset :: GenParser Char st Int
offset = do
  spaces
  (char '+' >> int) <|> (char '-' >> (fmap (* (-1)) int))


msg :: GenParser Char st String
msg = do
  spaces
  string "MSG "
  many1 (noneOf "\n")

data REM = REM PartialDate (Maybe Int) String deriving (Eq,Show)

rem :: GenParser Char st REM
rem = do
  spaces
  d <- date
  o <- optionMaybe offset
  m <- msg
  return (REM d o m)


parse :: String -> IO (Either ParseError RemFile)
parse path = do
  input <- readFile path
  return $ P.parse remFile path input


