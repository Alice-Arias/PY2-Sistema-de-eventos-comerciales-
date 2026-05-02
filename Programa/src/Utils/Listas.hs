module Utils.Listas where

contar :: Eq a => a -> [a] -> Int
contar elemento lista =
    length (filtrarIguales elemento lista)


filtrarIguales :: Eq a => a -> [a] -> [a]
filtrarIguales elemento =
    filter (== elemento)


maxPor :: Ord b => (a -> b) -> [a] -> a
maxPor funcionValor lista =
    foldl1 (compararPor funcionValor) lista


compararPor :: Ord b => (a -> b) -> a -> a -> a
compararPor funcionValor a b =
    if funcionValor a > funcionValor b
        then a
        else b