-- | a collection of parsers for types used in the config
module Caligraph.Config.Types where

import Data.Text (Text, pack, unpack)
import Data.Char
import Text.Read
import qualified Data.List.Split as Split

import Graphics.Vty.Input.Events (Modifier, Key(KChar,KFun))
import Graphics.Vty.Attributes

data PrettyKey = PrettyKey { prettyKey :: Key } deriving (Eq,Ord)

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

data PrettyColor = PrettyColor { prettyColor :: Color }

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
  show (PrettyColor color) =
    case findlist colornames color of
      Just s -> s
      Nothing -> "UnknownColor"

instance Read PrettyColor where
  readsPrec _ str = fmap (\c -> (PrettyColor c, "")) $
    case findlist (map swap colornames) str of
      Just color -> return color
      Nothing -> []
    where swap (a,b) = (b,a)


