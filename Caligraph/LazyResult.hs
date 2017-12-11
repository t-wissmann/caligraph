module Caligraph.LazyResult where

import Control.Monad
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as S
import Data.List

-- | a computation of res that may suspend by asking a question of type 'param'
-- waiting for an answer of type 'ri'
data LazyResult param ri res
    = Result res
    -- ^ a successfully finished computation
    | Recurse param (ri -> LazyResult param ri res)
    -- ^ a suspended computation

callback :: p -> LazyResult p ri ri
callback p = Recurse p Result

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

-- | Run a lazy computation
lazyRun
    :: (a -> b)
    -- ^ answer Recurse requests by this function
    -> LazyResult a b r
    -- ^ the computation
    -> r
    -- ^ the result
lazyRun _ (Result r) = r
lazyRun f (Recurse p cb) = lazyRun f (cb (f p))

lazyRunM
    :: Monad m
    => (a -> m b)
    -- ^ answer Recurse requests by this function
    -> LazyResult a b r
    -- ^ the computation
    -> m r
    -- ^ the result

lazyRunM _ (Result r) = return r
lazyRunM f (Recurse p cb) = do
    b <- f p
    lazyRunM f $ cb b


-- ** Playground and intuition
--
-- | read as many elements as possible
extractor :: LazyResult () (Maybe a) [a]
extractor = do
    val <- callback ()
    case val of
        Just val' -> do
            vals <- extractor
            return (val':vals)
        Nothing ->
            return []

-- | read the elements of a list successively
supply :: () -> State [a] (Maybe a)
supply () = do
    s <- S.get
    case s of
        [] -> return Nothing
        (x:xs) -> do
            S.put xs
            return (Just x)

-- | read non-empty lines from stdin
paragraph :: IO [String]
paragraph = lazyRunM (myGetLine) extractor
    where myGetLine :: () -> IO (Maybe String)
          myGetLine () = fmap (fmap (uncurry (:)) . uncons) getLine


