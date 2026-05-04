module Core.Promedios where

import Types.Modelos
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: calcularPromedios
-- Entrada: lista de eventos del sistema donde se registran todas las acciones
-- Salida: lista con el promedio de valor por cada categoría de evento
-- Restricciones:
--   - Si no hay eventos, devuelve una lista vacía
--   - Depende de funciones auxiliares de cálculo y filtrado
--------------------------------------------------------------------------------
calcularPromedios :: [Evento] -> [(Categoria, Float)]
calcularPromedios eventos =

    let 
        categoriasSinRepetir = obtenerCategoriasUnicas eventos
    in 
        map (calcularPromedioPorCategoria eventos) categoriasSinRepetir


--------------------------------------------------------------------------------
-- Nombre: calcularPromedioPorCategoria
-- Entrada:
--   eventos: lista completa de eventos del sistema
--   categoria: tipo de evento que se va a analizar
-- Salida:
--   par que contiene la categoría y su promedio de valor
-- Restricciones:
--   - Si no hay eventos en esa categoría, el promedio será 0
--------------------------------------------------------------------------------
calcularPromedioPorCategoria :: [Evento] -> Categoria -> (Categoria, Float)
calcularPromedioPorCategoria eventos categoriaActual =

    let 
        eventosDeCategoria = filtrarEventosPorCategoria eventos categoriaActual
        sumaDeValores    = sumarValores eventosDeCategoria
        totalEventos   = contarEventos eventosDeCategoria
        promedio     = calcularPromedio sumaDeValores totalEventos

    in (categoriaActual, promedio)