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

data PrettyKey = PrettyKey { prettyKey :: Key } deriving (Eq,Ord)

class UserReadShow a where
  userShow :: a -> String
  -- | parse to the given type, possibly not consuming all tokens
  userParser :: GenParser Char () a

userRead :: UserReadShow a => String -> Either String a
userRead str =
  left show $ parse (do x <- userParser; eof; return x) "" str

-- userReadAuto :: Read a => String -> String -> Either String a
-- userReadAuto typeName str =
--   case (readEither str) of
--     Left _ -> Left ("not a valid " ++ typeName)
--     Right x -> Right x
-- 
instance UserReadShow Int where
  userShow = show
  userParser = read <$> many1 digit

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

instance UserReadShow Key where
  userShow k = case k of
        KChar '-' -> "minus"
        KChar ch -> [ch]
        KFun int -> ('F' : show int)
        _ -> drop 1 (show k) -- drop the 'K'
  userParser =
        (string "minus" >> return (KChar '-'))
        <|> (char '@' >> ((KChar . chr) <$> userParser))
        <|> (char 'F' >> (KFun <$> userParser))
        <|> try (do
          c <- anyToken
          return $ KChar c)
        <|> try (do
          str <- many1 alphaNum
          case readEither ('K':str) of
            Right x -> return (x :: Key)
            Left _ -> fail ("Unknown key name " ++ str)
          )

instance Show PrettyKey where
    show (PrettyKey k) = case k of
        KChar '-' -> "minus"
        KChar ch -> [ch]
        KFun int -> ('F' : show int)
        _ -> drop 1 (show k) -- drop the 'K'

instance Read PrettyKey where
    readsPrec _ str = fmap (\x -> (PrettyKey x, "")) $
            let len = length str in
            if str == "minus"
            then [KChar '-']
            else
                if len == 1
                then [KChar (str !! 0)]
                else
                    case str of
                    ('@':intStr) -> do
                        (keycode,_) <- readsPrec 0 intStr
                        return $ KChar $ chr keycode
                    ('F':intStr) -> do
                        (int,_) <- readsPrec 0 intStr
                        return $ KFun int
                    _ -> do
                        fmap fst $ readsPrec 0 ('K' : str)

data PrettyModifier = PrettyModifier { prettyModifier :: Modifier } deriving (Eq,Ord)

instance Show PrettyModifier where
    show (PrettyModifier m) = drop 1 $ show m -- drop the first M

instance Read PrettyModifier where
    readsPrec k str = do
        (m,r) <- readsPrec k ('M':str)
        return (PrettyModifier m, r)

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

instance Show KeyCombi where
    show (KeyCombi (mods, key)) =
        foldl1 (\x y -> x ++ ['-'] ++ y) $
        map (show . PrettyModifier) mods ++ [show $ PrettyKey key]

instance Read KeyCombi where
    readsPrec _ str =
        case reverse $ Split.splitOn "-" str of
            [] -> [] -- no parse
            (keyStr: mods) ->
                fmap (\x -> (KeyCombi x,"")) $ do
                    modsM <- mapM (readsPrec 0) (reverse mods)
                    (key,_) <- readsPrec 0 keyStr
                    return (fmap (prettyModifier . fst) modsM, prettyKey key)

data PrettyColor = PrettyColor { prettyColor :: Maybe Color }

colornames :: [(Color,String)]
colornames =
  [ (,) black "black"
  , (,) red "red"
  , (,) green "green"
  , (,) yellow "yellow"
  , (,) blue "blue"
  , (,) magenta "magenta"
  , (,) cyan "cyan"
  , (,) white "white"
  , (,) brightBlack "brightblack"
  , (,) brightRed "brightred"
  , (,) brightGreen "brightgreen"
  , (,) brightYellow "brightyellow"
  , (,) brightBlue "brightblue"
  , (,) brightMagenta "brightmagenta"
  , (,) brightCyan "brightcyan"
  , (,) brightWhite "brightwhite"
  ]

findlist :: Eq a => [(a,b)] -> a -> Maybe b
findlist [] _ = Nothing
findlist ((a,b):xs) a' =
  if a == a' then Just b else findlist xs a'

instance Show PrettyColor where
  show (PrettyColor (Just color)) =
    case findlist colornames color of
      Just s -> s
      Nothing -> "UnknownColor"
  show (PrettyColor (Nothing)) = "default"

instance Read PrettyColor where
  readsPrec _ str = fmap (\c -> (PrettyColor c, "")) $
    if str == "default" then [Nothing] else
    case findlist (map swap colornames) str of
      Just color -> return (Just color)
      Nothing ->
        case str of
        ('#':hexdesc) ->
          (either (const []) return ) $ do
            (r,g,b) <- parseHexBytes hexdesc >>= expectTriple
            return $ Just $ rgbColor r g (b :: Int)
        _ -> []
    where swap (a,b) = (b,a)

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



