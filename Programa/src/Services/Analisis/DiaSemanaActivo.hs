module Services.Analisis.DiaSemanaActivo where

import Types.Modelos
import Data.List 
import Data.Ord (comparing)
--------------------------------------------------------------------------------
-- Nombre: nombreDiaSemana
-- Entrada: número del día 
-- Salida: nombre del día de la semana en texto
-- Restricciones:
--   - Si el número no coincide con 1–6, se considera domingo por defecto
--------------------------------------------------------------------------------
nombreDiaSemana :: Int -> String
nombreDiaSemana numeroDia =
    case numeroDia of
        1 -> "Lunes"
        2 -> "Martes"
        3 -> "Miércoles"
        4 -> "Jueves"
        5 -> "Viernes"
        6 -> "Sábado"
        _ -> "Domingo"


--------------------------------------------------------------------------------
-- Nombre: obtenerIndiceDia
-- Entrada: evento del sistema
-- Salida: número que representa el día de la semana del evento
-- Restricciones:
--   - Depende del timestamp del evento
--------------------------------------------------------------------------------
obtenerIndiceDia :: Evento -> Int
obtenerIndiceDia evento = timestamp evento `mod` 7 -- el 7 es un número arbitrario para simular días de la semana


--------------------------------------------------------------------------------
-- Nombre: obtenerDiasUnicos
-- Entrada: lista de eventos del sistema
-- Salida: lista de días únicos en los que ocurrieron eventos
-- Restricciones:
--   - Elimina días repetidos automáticamente
--------------------------------------------------------------------------------
obtenerDiasUnicos :: [Evento] -> [Int]
obtenerDiasUnicos eventos =

    let dias = map obtenerIndiceDia eventos

    in nub dias
--------------------------------------------------------------------------------
-- Nombre: filtrarEventosPorDia
-- Entrada:
--   dia: número del día de la semana
--   eventos: lista de eventos del sistema
-- Salida: lista de eventos que ocurrieron en ese día
-- Restricciones:
--   - Depende del cálculo de índice de día
--------------------------------------------------------------------------------
filtrarEventosPorDia :: Int -> [Evento] -> [Evento]
filtrarEventosPorDia dia =

    let esDelDia evento = obtenerIndiceDia evento == dia
    
    in filter esDelDia
--------------------------------------------------------------------------------
-- Nombre: contarEventosPorDia
-- Entrada:
--   dia: número del día de la semana
--   eventos: lista de eventos del sistema
-- Salida: cantidad total de eventos en ese día
-- Restricciones:
--   - Ninguna
--------------------------------------------------------------------------------
contarEventosPorDia :: Int -> [Evento] -> Int
contarEventosPorDia dia eventos =

    let eventosDelDia = filtrarEventosPorDia dia eventos

    in length eventosDelDia

--------------------------------------------------------------------------------
-- Nombre: construirResumenDia
-- Entrada:
--   dia: número del día
--   eventos: lista de eventos del sistema
-- Salida:
--   par con nombre del día y cantidad de eventos ocurridos
-- Restricciones:
--   - Usa conversión de número a nombre de día
--------------------------------------------------------------------------------
construirResumenDia :: Int -> [Evento] -> (String, Int)
construirResumenDia dia eventos =

    let nombreDia = nombreDiaSemana dia

        cantidadEventos = contarEventosPorDia dia eventos

    in (nombreDia, cantidadEventos)


--------------------------------------------------------------------------------
-- Nombre: construirResumenGeneral
-- Entrada:
--   dias: lista de días encontrados en el sistema
--   eventos: lista de eventos del sistema
-- Salida: lista con resumen de actividad por día
-- Restricciones:
--   - Cada día se analiza una sola vez
--------------------------------------------------------------------------------
construirResumenGeneral :: [Int] -> [Evento] -> [(String, Int)]
construirResumenGeneral dias eventos =

    let resumenDia dia = construirResumenDia dia eventos

    in map resumenDia dias

--------------------------------------------------------------------------------
-- Nombre: diaMasActivo
-- Entrada: lista de eventos del sistema
-- Salida:
--   día de la semana con más eventos y la cantidad total
-- Restricciones:
--   - La lista no debería estar vacía para obtener resultado correcto
--------------------------------------------------------------------------------
diaMasActivo :: [Evento] -> (String, Int)
diaMasActivo eventos =

    let dias = obtenerDiasUnicos eventos

        resumen = construirResumenGeneral dias eventos

    in maximumBy (comparing snd) resumen