---------------------------------------------------------------------
-- A caligraph command is a parser helper such that the user can bind
-- keys to commands with arguments
---------------------------------------------------------------------

module Caligraph.Config.Command where

import Control.Monad (ap)

data CommandArgParser target
    = NoMoreArguments target
    | MandatoryArgument (String -> CommandArgParser target)

instance Functor CommandArgParser where
    fmap f (NoMoreArguments p) = NoMoreArguments $ f p
    fmap f (MandatoryArgument p) = MandatoryArgument $ fmap (fmap f) p

instance Applicative CommandArgParser where
    pure = NoMoreArguments
    (<*>) = ap

instance Monad CommandArgParser where
    return = pure
    (NoMoreArguments a) >>= a_to_mb = a_to_mb a
    (MandatoryArgument f) >>= a_to_mb =
        MandatoryArgument (\s -> (f s) >>= a_to_mb)

readArg :: Read a => CommandArgParser a
readArg = MandatoryArgument (return . read)


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
