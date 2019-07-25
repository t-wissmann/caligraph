module Caligraph.Breakpoint where

import Caligraph.Possibly

import Control.Monad.Trans.Class
import Control.Monad.IO.Class


data Breakpoint m a = Breakpoint {
    projBreakpoint :: (m (Possibly m a))
  }

discardBreakpoint :: Monad m => Breakpoint m a -> m a
discardBreakpoint (Breakpoint mx) = do
    x <- mx
    case x of
        Pure y -> return y
        Monadic my -> my

instance Functor m => Functor (Breakpoint m) where
    fmap f (Breakpoint mx) = Breakpoint (fmap (fmap f) mx)

instance Applicative m => Applicative (Breakpoint m) where
    pure = Breakpoint . pure . pure
    Breakpoint f <*> Breakpoint x = Breakpoint (t2 f x)
        where
          t2 :: (Applicative f2, Applicative f1) => f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
          t2 = (<*>) . ((<*>) (pure (<*>)))

instance Monad m => Monad (Breakpoint m) where
    (Breakpoint mx) >>= f = Breakpoint $ do
        -- do in the monad m
        possibly_mx <- mx
        case possibly_mx of
            Pure x -> projBreakpoint (f x)
            Monadic mx' ->
                return $ Monadic $ do
                    x <- mx'
                    discardBreakpoint (f x)

instance MonadTrans Breakpoint where
    lift action = Breakpoint (fmap return action)

instance MonadIO m => MonadIO (Breakpoint m) where
    liftIO io_action = Breakpoint (fmap return $ liftIO io_action)

breakMonad :: Monad m => Breakpoint m ()
breakMonad = Breakpoint $ return $ Monadic $ return ()

