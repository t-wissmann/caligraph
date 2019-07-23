---------------------------------------------------------------------
-- A caligraph command is a parser helper such that the user can bind
-- keys to commands with arguments
---------------------------------------------------------------------

module Caligraph.Config.Command where

import Control.Monad (ap)
import Text.Read (readEither)
import Text.Printf (printf)

data CommandArgParser target
    = NoMoreArguments target
    | MandatoryArgument (String -> Either String (CommandArgParser target))

instance Functor CommandArgParser where
    fmap f (NoMoreArguments p) = NoMoreArguments $ f p
    fmap f (MandatoryArgument p) = MandatoryArgument $ fmap (fmap (fmap f)) p

instance Applicative CommandArgParser where
    pure = NoMoreArguments
    (<*>) = ap

instance Monad CommandArgParser where
    return = pure
    (NoMoreArguments a) >>= a_to_mb = a_to_mb a
    (MandatoryArgument f) >>= a_to_mb =
        MandatoryArgument (\s -> do
            l <- (f s)
            return (l >>= a_to_mb))

readArg :: Read a => CommandArgParser a
readArg = MandatoryArgument (\s -> fmap return $ readEither s)


-- | bind and parse the arguments to obtain sth of target type
bind :: CommandArgParser target -> [String] -> Either String target
bind = bindCount 0
  where
    bindCount :: Int -> CommandArgParser target -> [String] -> Either String target
    bindCount n (NoMoreArguments t) [] = return t
    bindCount n (NoMoreArguments _) l =
        Left
          $ printf "Too many arguments given (expected %d, given %d)"
            n (n + length l)
    bindCount n (MandatoryArgument _) [] =
        Left
          $ printf "Too few arguments given (given %d, expected more)" n
    bindCount n (MandatoryArgument clos) (x:xs) = do
        nextParser <- clos x
        bindCount (n+1) nextParser xs

-- to test suite
noArgs :: IO ()
noArgs = return ()

oneArg :: Int -> IO ()
oneArg x = return ()

twoArgs :: String -> Int -> IO ()
twoArgs x y = return ()

functions :: [CommandArgParser (IO ())]
functions =
    [ return noArgs
    , return oneArg <*> readArg
    , return twoArgs <*> readArg <*> readArg
    ]
