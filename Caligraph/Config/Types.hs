{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | a collection of parsers for types used in the config
module Caligraph.Config.Types where

import Data.Text (Text, pack, unpack)
import Data.Char
import Data.List
import Text.Read (readEither)
import qualified Data.List.Split as Split

import qualified Data.HashMap.Strict as M

import Text.ParserCombinators.Parsec
import Control.Arrow (left)

import Graphics.Vty.Input.Events (Modifier, Key(KChar,KFun))
import qualified Graphics.Vty.Input.Events  as Event
import Graphics.Vty.Attributes

type SectionParser a = M.HashMap Text Text -> Either String a

class UserReadShow a where
  userShow :: a -> String
  -- | parse to the given type, possibly not consuming all tokens
  userParser :: GenParser Char () a

userRead :: UserReadShow a => String -> Either String a
userRead str =
  left show
    $ parse (do x <- userParser; eof; return x)
            ("token \"" ++ str ++ "\"") str

-- userReadAuto :: Read a => String -> String -> Either String a
-- userReadAuto typeName str =
--   case (readEither str) of
--     Left _ -> Left ("not a valid " ++ typeName)
--     Right x -> Right x
-- 
instance UserReadShow Int where
  userShow = show
  userParser = read <$> many1 (oneOf "-0123456789")

instance UserReadShow Integer where
  userShow = show
  userParser = read <$> many1 (oneOf "-0123456789")

instance UserReadShow Double where
  userShow = show
  userParser = read <$> many1 (oneOf "-0123456789.")

-- instance UserReadShow Double where
--   userShow = show
--   userRead = userReadAuto "floating point number"

class FinitelyManyNames a where
  -- | list of names. Elements of a are allowed to occur multiple times
  -- but for showing, only the first entry is used
  finitelyManyNames :: [(String, a)]

showName :: (Eq a, FinitelyManyNames a) => a -> String
showName x = case find (\(_,v) -> v == x) finitelyManyNames of
  Just (name,_) -> name
  Nothing -> error "Value does not appear in finitelyManyNames"

parseName :: FinitelyManyNames a => GenParser Char () a
parseName =
  choice $ map (\(n,v) -> string n >> return v) finitelyManyNames

instance FinitelyManyNames Bool where
  finitelyManyNames =
    [ (,) "true" True
    , (,) "false" False
    , (,) "on" True
    , (,) "off" False
    ]
instance UserReadShow Bool where
  userShow = showName
  userParser = parseName

instance UserReadShow String where
  userShow = id
  userParser = many anyToken

instance UserReadShow Key where
  userShow k = case k of
        KChar '-' -> "minus"
        KChar ' ' -> "space"
        KChar ch -> [ch]
        KFun int -> ('F' : show int)
        _ -> drop 1 (show k) -- drop the 'K'
  userParser =
        try (string "minus" >> return (KChar '-'))
        <|> try (string "space" >> return (KChar ' '))
        <|> try (char '@' >> ((KChar . chr) <$> userParser))
        <|> try (char 'F' >> (KFun <$> userParser))
        <|> try (do
          str <- many1 alphaNum
          case readEither ('K':str) of
            Right x -> return (x :: Key)
            Left _ -> fail ("Unknown key name " ++ str)
          )
        <|> try (do
          c <- anyToken
          return $ KChar c)

instance FinitelyManyNames Modifier where
  finitelyManyNames =
    [ (,) "Shift"   Event.MShift
    , (,) "Ctrl"    Event.MCtrl
    , (,) "Meta"    Event.MMeta
    , (,) "Control" Event.MCtrl
    , (,) "Alt"     Event.MMeta
    ]

instance UserReadShow Modifier where
  userShow = showName
  userParser = parseName

data KeyCombi = KeyCombi {keyCombi :: ([Modifier], Key)} deriving (Eq,Ord) -- a list of modifiers and a key

instance UserReadShow KeyCombi where
  userShow (KeyCombi (mods, key)) =
        foldl1 (\x y -> x ++ ['-'] ++ y) $
        map userShow mods ++ [userShow key]

  userParser = do
    mods <- userParser `endBy` char '-'
    key <- userParser
    return $ KeyCombi (mods,key)

instance FinitelyManyNames Color where
  finitelyManyNames =
    [ (,) "black"         black
    , (,) "red"           red
    , (,) "green"         green
    , (,) "yellow"        yellow
    , (,) "blue"          blue
    , (,) "magenta"       magenta
    , (,) "cyan"          cyan
    , (,) "white"         white
    , (,) "brightblack"   brightBlack
    , (,) "brightred"     brightRed
    , (,) "brightgreen"   brightGreen
    , (,) "brightyellow"  brightYellow
    , (,) "brightblue"    brightBlue
    , (,) "brightmagenta" brightMagenta
    , (,) "brightcyan"    brightCyan
    , (,) "brightwhite"   brightWhite
    ]

instance UserReadShow Color where
  userShow = showName
  userParser =
    (do
      char '#'
      [r,g,b] <- count 3 byte
      return $ rgbColor r g b
    )
    <|> parseName
    where byte = do
            a <- oneOf "0123456789abcdefABCDEF"
            b <- oneOf "0123456789abcdefABCDEF"
            return (read ['0','x',a,b] :: Int)

-- | type of color settings in the ui. "Nothing" is
-- the default color
data UiColor = UiColor { uiColor :: Maybe Color }

instance UserReadShow UiColor where
  userShow (UiColor Nothing) = "default"
  userShow (UiColor (Just x)) = userShow x
  userParser =
    try (string "default" >> (return $ UiColor Nothing))
    <|> (UiColor <$> Just <$> userParser)

findlist :: Eq a => [(a,b)] -> a -> Maybe b
findlist [] _ = Nothing
findlist ((a,b):xs) a' =
  if a == a' then Just b else findlist xs a'

-- parseHexBytes :: (Read i, Integral i) => String -> Either String [i]
parseHexBytes :: String -> Either String [Int]
parseHexBytes str =
  case str of
    (x1:x2:xs) -> do
      pure (:) <*> readEither ['0','x',x1,x2] <*> parseHexBytes xs
    [] -> Right []
    (_:[]) -> Left "A hex byte string must be of even length"

expectTriple :: [a] -> Either String (a,a,a)
expectTriple [a,b,c] = return (a,b,c)
expectTriple s = Left ("Expected 3 values but got " ++ show (length s))



