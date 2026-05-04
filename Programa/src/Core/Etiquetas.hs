module Core.Etiquetas where

import Types.Modelos
import Core.Promedios
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: etiquetarAltoValor
--
-- Objetivo: marca cada evento indicando si su valor es mayor al promedio
--           de su categoría
--
-- Entradas: lista de eventos del sistema
--
-- Salida: lista de eventos con el campo "etiqueta" actualizado
--
-- Restricciones:
--   - Depende del cálculo de promedios por categoría
--   - Si la lista está vacía, retorna una lista vacía
--------------------------------------------------------------------------------
etiquetarAltoValor :: [Evento] -> [Evento]
etiquetarAltoValor eventos =
    let 
        promediosPorCategoria = calcularPromedios eventos
    in 
        map (etiquetarEvento promediosPorCategoria) eventos

--------------------------------------------------------------------------------
-- Nombre: etiquetarEvento
--
-- Objetivo: asigna a un evento una etiqueta según su valor respecto al promedio
--
-- Entradas:
--   - lista de promedios por categoría
--   - evento individual
--
-- Salida: evento con campo "etiqueta" actualizado
--
-- Restricciones:
--   - requiere que existan promedios previamente calculados
--------------------------------------------------------------------------------
etiquetarEvento :: [(Categoria, Float)] -> Evento -> Evento
etiquetarEvento promediosPorCategoria evento = evento { etiqueta = esAltoValor promediosPorCategoria evento }

--------------------------------------------------------------------------------
-- Nombre: esAltoValor
--
-- Objetivo: determina si el valor de un evento supera el promedio de su categoría
--
-- Entradas:
--   - lista de promedios por categoría
--   - evento individual
--
-- Salida:
--   True si el valor del evento es mayor al promedio de su categoría
--   False si no lo es o si no existe promedio para la categoría
--
-- Restricciones:
--   - si la categoría no tiene promedio registrado, se asume False
--------------------------------------------------------------------------------
esAltoValor :: [(Categoria, Float)] -> Evento -> Bool
esAltoValor promediosPorCategoria evento =
    case obtenerPromedioCategoria promediosPorCategoria (categoria evento) of

        Nothing ->

            False

        Just promedio ->

            let valorEvento = valor evento

            in valorEvento > promedio