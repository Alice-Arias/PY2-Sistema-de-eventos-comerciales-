module Utils.Listas where

--------------------------------------------------------------------------------
-- Nombre: contarElemento
-- Entrada:
--   elemento: valor a buscar en la lista
--   lista: lista de elementos
-- Salida:
--   cantidad de veces que aparece el elemento
-- Restricciones:
--   - Usa filtrado para contar coincidencias
--------------------------------------------------------------------------------
contarElemento :: Eq a => a -> [a] -> Int
contarElemento elemento lista =
    length (filtrarElemento elemento lista)

--------------------------------------------------------------------------------
-- Nombre: filtrarElemento
-- Entrada:
--   elemento: valor a filtrar
--   lista: lista de elementos
-- Salida:
--   lista con los elementos iguales al buscado
-- Restricciones:
--   - Filtra usando igualdad exacta
--------------------------------------------------------------------------------
filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento elemento =
    filter (== elemento)

--------------------------------------------------------------------------------
-- Nombre: maximoPorCriterio
-- Entrada:
--   criterio: función que transforma cada elemento en un valor comparable
--   lista: lista de elementos
-- Salida:
--   elemento con el valor máximo según el criterio
-- Restricciones:
--   - Usa foldl1 para recorrer la lista
--   - La lista no debe estar vacía
--------------------------------------------------------------------------------
maximoPorCriterio :: Ord b => (a -> b) -> [a] -> a
maximoPorCriterio criterio = foldl1 (compararPorCriterio criterio)

--------------------------------------------------------------------------------
-- Nombre: compararPorCriterio
-- Entrada:
--   criterio: función de comparación
--   a: primer elemento
--   b: segundo elemento
-- Salida:
--   el elemento mayor según el criterio
-- Restricciones:
--   - Compara usando valores derivados del criterio
--------------------------------------------------------------------------------
compararPorCriterio :: Ord b => (a -> b) -> a -> a -> a
compararPorCriterio criterio a b =
    if criterio a > criterio b
        then a
        else b