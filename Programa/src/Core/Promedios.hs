module Core.Promedios where

import Types.Evento
import Types.Categoria
import Data.List (nub)

calcularPromedios :: [Evento] -> [(Categoria, Float)]
calcularPromedios eventos =
    let 
        categoriasUnicas = nub (map categoria eventos)
    in 
        map (promedioPorCategoria eventos) categoriasUnicas


promedioPorCategoria :: [Evento] -> Categoria -> (Categoria, Float)
promedioPorCategoria eventos categoriaActual =
    let 
        eventosCat = filter (\e -> categoria e == categoriaActual) eventos
        suma = sum (map valor eventosCat)
        cantidad = length eventosCat
        promedio = 
            if cantidad == 0 
            then 0 
            else suma / fromIntegral cantidad
    in 
        (categoriaActual, promedio)