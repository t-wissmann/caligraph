module Caligraph.Utils where

import Data.Array as A

lastSafe :: [a] -> Maybe a
lastSafe = f Nothing
  where f :: Maybe a -> [a] -> Maybe a
        f _   (x:xs) = f (Just x) xs
        f acc [] = acc

safeArray :: Ix i => A.Array i e -> i -> Maybe e
safeArray arr i =
  if (f <= i && i <= t)
  then Just (arr ! i)
  else Nothing
  where
    (f,t) = A.bounds arr
