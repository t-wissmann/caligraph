module Caligraph.Possibly where

import Control.Monad.State

-- Either a pure or monadic computation
data Possibly m a = Pure a | Monadic (m a)

instance Functor m => Functor (Possibly m) where
    fmap f (Pure x) = Pure (f x)
    fmap f (Monadic mx) = Monadic $ fmap f mx

instance Applicative m => Applicative (Possibly m) where
    pure = Pure
    Pure f <*> Pure x = Pure (f x)
    Pure f <*> Monadic x = Monadic $ pure f <*> x
    Monadic f <*> Pure x = Monadic $ f <*> pure x
    Monadic f <*> Monadic x = Monadic $ f <*> x

instance Monad m => Monad (Possibly m) where
    Pure x >>= f = f x
    Monadic mx >>= f = Monadic $ do
        x <- mx
        case f x of
            Pure x -> return x
            Monadic x -> x

instance MonadTrans Possibly where
    lift = Monadic

instance MonadIO m => MonadIO (Possibly m) where
    liftIO = Monadic . liftIO

