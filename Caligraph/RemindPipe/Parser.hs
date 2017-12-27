module Caligraph.RemindPipe.Parser where

import Caligraph.RemindPipe.Types
import qualified Caligraph.Backend.Types as CB

import Data.Time.Calendar
import qualified Data.List.Split as Split
import Data.Either

import Text.ParserCombinators.Parsec hiding (parse,optional)
import Text.ParserCombinators.Parsec.Token
import qualified Text.ParserCombinators.Parsec as P

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

parseRemOutput :: String -> [(Day,CB.Incarnation Identifier)]
parseRemOutput buffer = rights $ map parseIncarnation $ pair $ Split.endBy "\n" buffer
  where
    pair :: [a] -> [(a,a)]
    pair [] = []
    pair (a:b:tl) = (a,b) : pair tl
    pair (_:[]) = []

parseIncarnation :: (String,String) -> Either String (Day,CB.Incarnation Identifier)
parseIncarnation (metadata,fields) =
    mapLeft show $ P.parse (main_fields ("",0)) "remind output" fields

main_fields :: ident -> GenParser Char st (Day,CB.Incarnation ident)
main_fields identifier = do
    day <- dateSlashISO
    char ' '
    char '*'
    char ' '
    char '*'
    char ' '
    duration <- fmap (fmap minutesToTime) (optional parseNum)
    char ' '
    time <- fmap (fmap minutesToTime) (optional parseNum)
    case time of
        Nothing -> char ' '
        Just _ -> do
            -- if the reminder is a timed reminder,
            -- then ignore the next word
            char ' '
            many1 (noneOf " ")
            char ' '
    title <- many1 (noneOf "\n")
    return
        $ (,) day
        $ CB.Incarnation day time duration title identifier

-- Convert minutes after midnight to the time of a day
minutesToTime :: Integral a => a -> (a,a)
minutesToTime a = (a `div` 60, a `mod` 60)

optional :: GenParser Char st a -> GenParser Char st (Maybe a)
optional parser = do
    (char '*' >> return Nothing) <|> fmap Just parser

parseNum :: Read a => GenParser Char st a
parseNum = read <$> many1 digit

dateSlashISO :: GenParser Char st Day
dateSlashISO = do
  y <- parseNum
  char '/'
  m <- parseNum
  char '/'
  d <- parseNum
  return $ fromGregorian y m d

