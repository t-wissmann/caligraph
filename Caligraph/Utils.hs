module Caligraph.Utils where


lastSafe :: [a] -> Maybe a
lastSafe = f Nothing
  where f :: Maybe a -> [a] -> Maybe a
        f _   (x:xs) = f (Just x) xs
        f acc [] = acc

