module Caligraph.LazyResult where

import Control.Monad

-- | a computation of res that may suspend by asking a qeustion of type 'param'
-- waiting for an answer of type 'ri'
data LazyResult param ri res
    = Result res
    -- ^ a successfully finished computation
    | Recurse param (ri -> LazyResult param ri res)
    -- ^ a suspended computation

instance Functor (LazyResult p ri) where
  fmap f (Result res) = Result (f res)
  fmap f (Recurse p cl) = Recurse p (fmap f . cl)

instance Applicative (LazyResult p ri) where
  pure = Result
  (<*>) = ap

instance Monad (LazyResult p ri) where
  (>>=) (Result res) a2lrb = a2lrb res
  (>>=) (Recurse p i2res) a2lrb =
    Recurse p (\ri -> (i2res ri) >>= a2lrb)
  return = Result

