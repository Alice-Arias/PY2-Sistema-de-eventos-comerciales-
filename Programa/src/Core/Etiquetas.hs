module Core.Etiquetas where

import Types.Modelos
import Core.Promedios
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: etiquetarAltoValor
-- Entrada: eventos: lista de registros del sistema con información de acciones realizadas
-- Salida: lista de eventos donde cada uno queda marcado si tiene un valor alto o no
-- Restricciones:
--   Depende del cálculo de promedios por categoría
--   La lista puede estar vacía, en ese caso devuelve una lista vacía
--------------------------------------------------------------------------------
etiquetarAltoValor :: [Evento] -> [Evento]
etiquetarAltoValor eventos =
    let 
        promediosPorCategoria = calcularPromedios eventos
    in 
        map (etiquetarEvento promediosPorCategoria) eventos

--------------------------------------------------------------------------------
-- Nombre: etiquetarEvento
-- Entrada:
--   promediosPorCategoria: lista con el promedio de valor por cada categoría
--   evento: registro individual del sistema
-- Salida:
--   mismo evento, pero con una etiqueta que indica si su valor es alto o no
-- Restricciones:
--   requiere que exista información de promedios calculada previamente
--------------------------------------------------------------------------------
etiquetarEvento :: [(Categoria, Float)] -> Evento -> Evento
etiquetarEvento promediosPorCategoria evento = evento { etiqueta = esAltoValor promediosPorCategoria evento }

--------------------------------------------------------------------------------
-- Nombre: esAltoValor
-- Entrada:
--   promediosPorCategoria: lista con el promedio de valor por categoría
--   evento: registro individual del sistema
-- Salida:
--   verdadero si el valor del evento es mayor al promedio de su categoría
--   falso en caso contrario
-- Restricciones:
--   si no existe promedio para la categoría, se considera como falso
--------------------------------------------------------------------------------
esAltoValor :: [(Categoria, Float)] -> Evento -> Bool
esAltoValor promediosPorCategoria evento =

    case obtenerPromedioCategoria promediosPorCategoria (categoria evento) of

        Nothing       -> False
        
        Just promedio -> valor evento > promedio