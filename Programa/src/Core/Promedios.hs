module Core.Promedios where

import Types.Modelos
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: calcularPromedios
--
-- Objetivo: calcula el promedio del valor de los eventos agrupados por categoría
--
-- Entradas: lista de eventos del sistema
--
-- Salida: lista de pares (categoría, promedio de valor)
--
-- Restricciones:
--   - Si no hay eventos, se devuelve una lista vacía
--   - Depende de funciones auxiliares de filtrado y cálculo
--------------------------------------------------------------------------------
calcularPromedios :: [Evento] -> [(Categoria, Float)]
calcularPromedios eventos =

    let 
        categoriasSinRepetir = obtenerCategoriasUnicas eventos
    in 
        map (calcularPromedioPorCategoria eventos) categoriasSinRepetir


--------------------------------------------------------------------------------
-- Nombre: calcularPromedioPorCategoria
--
-- Objetivo: calcula el promedio del valor de los eventos de una categoría específica
--
-- Entradas:
--   - lista completa de eventos
--   - categoría a analizar
--
-- Salida: par (categoría, promedio de valor)
--
-- Restricciones:
--   - Si no hay eventos en la categoría, el promedio es 0
--------------------------------------------------------------------------------
calcularPromedioPorCategoria :: [Evento] -> Categoria -> (Categoria, Float)
calcularPromedioPorCategoria eventos categoriaActual =

    let 
        eventosDeCategoria = filtrarEventosPorCategoria eventos categoriaActual
        sumaDeValores    = sumarValores eventosDeCategoria
        totalEventos   = contarEventos eventosDeCategoria
        promedio     = calcularPromedio sumaDeValores totalEventos

    in (categoriaActual, promedio)