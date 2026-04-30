module Utils.Listas where

contar :: Eq a => a -> [a] -> Int
contar x = length . filter (== x)

maxPor :: Ord b => (a -> b) -> [a] -> a
maxPor f = foldl1 (\a b -> if f a > f b then a else b)