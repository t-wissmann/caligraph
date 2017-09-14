
module LazyResultSpec where
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as S
import Caligraph.LazyResult
import Tester

lazyMap :: [a] -> LazyResult a b [b]
lazyMap [] = return []
lazyMap (x:xs) = do
    y <- callback x
    ys <- lazyMap xs
    return (y:ys)

-- | test the above lazyMap against map
vsMap :: (Show b, Eq b) => [a] -> (a -> b) -> TestM ()
vsMap xs f = do
    (map f xs) =!= (lazyRun f $ lazyMap xs)

juggle :: (Eq a, Show a) => [a] -> TestM ()
juggle xs =
    (xs, []) =!= (S.runState (lazyRunM supply extractor) xs)

testLazyResult :: TestM ()
testLazyResult = do
    vsMap [1,4,3,5] (* 3)
    juggle "sdklxcmvoiw34ercksdjf"


