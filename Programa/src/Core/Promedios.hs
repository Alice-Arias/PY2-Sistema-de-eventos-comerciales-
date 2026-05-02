module Core.Promedios where

import Types.Evento
import Types.Categoria
import Data.List (nub)

calcularPromedios :: [Evento] -> [(Categoria, Float)]
calcularPromedios listaEventos =
    let categoriasUnicas = obtenerCategoriasUnicas listaEventos
    in map (calcularPromedioCategoria listaEventos) categoriasUnicas

calcularPromedioCategoria :: [Evento] -> Categoria -> (Categoria, Float)
calcularPromedioCategoria listaEventos categoriaActual =
    let eventosFiltrados = filtrarEventosPorCategoria listaEventos categoriaActual
        sumaValores      = sumarValores eventosFiltrados
        cantidadEventos  = contarEventos eventosFiltrados
        promedioFinal    = calcularPromedio sumaValores cantidadEventos
    in (categoriaActual, promedioFinal)


obtenerCategoriasUnicas :: [Evento] -> [Categoria]
obtenerCategoriasUnicas listaEventos = nub (map categoria listaEventos)

filtrarEventosPorCategoria :: [Evento] -> Categoria -> [Evento]
filtrarEventosPorCategoria listaEventos categoriaBuscada = filter (\evento -> categoria evento == categoriaBuscada) listaEventos

sumarValores :: [Evento] -> Float
sumarValores eventos = sum (map valor eventos)

contarEventos :: [Evento] -> Int
contarEventos = length

calcularPromedio :: Float -> Int -> Float
calcularPromedio suma cantidad =
    if cantidad == 0
        then 0
        else suma / fromIntegral cantidad