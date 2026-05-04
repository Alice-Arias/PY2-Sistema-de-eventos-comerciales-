module Services.Analisis.ResumenIntervalos where

import Types.Modelos
import Types.Fecha
import Data.List 
import Utils.Formato
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: resumenIntervalos
--
-- Objetivo: genera un resumen de actividad por mes y año
--
-- Entradas: lista de eventos del sistema
--
-- Salida:
--   lista de tuplas (periodo, cantidad de eventos, monto total)
--
-- Restricciones:
--   - los eventos deben tener fecha válida
--------------------------------------------------------------------------------
resumenIntervalos :: [Evento] -> [(String, Int, Float)]
resumenIntervalos eventos =
    let 
        periodosOrdenados = obtenerPeriodosOrdenados eventos
    in 
        map (calcularResumenDePeriodo eventos) periodosOrdenados


--------------------------------------------------------------------------------
-- Nombre: obtenerPeriodosOrdenados
--
-- Objetivo: obtiene los periodos (año, mes) sin repetir y ordenados
--
-- Entradas: lista de eventos
--
-- Salida: lista ordenada de pares (año, mes)
--
-- Restricciones:
--   - requiere fechas válidas en los eventos
--------------------------------------------------------------------------------
obtenerPeriodosOrdenados :: [Evento] -> [(Int, Int)]
obtenerPeriodosOrdenados eventos =
    let periodos = obtenerPeriodosUnicos eventos
    in sortOn (\(anio, mes) -> (anio, mes)) periodos


--------------------------------------------------------------------------------
-- Nombre: obtenerPeriodosUnicos
--
-- Objetivo: obtiene todos los (año, mes) sin repetición
--
-- Entradas: lista de eventos
--
-- Salida: lista sin duplicados
--
-- Restricciones: ninguna
--------------------------------------------------------------------------------
obtenerPeriodosUnicos :: [Evento] -> [(Int, Int)]
obtenerPeriodosUnicos eventos = nub [ (extraerAnio evento, extraerMes evento) | evento <- eventos ]


--------------------------------------------------------------------------------
-- Nombre: calcularResumenDePeriodo
--
-- Objetivo: genera el resumen completo de un periodo específico
--
-- Entradas:
--   - lista de eventos
--   - periodo (año, mes)
--
-- Salida: (nombre del periodo, cantidad, monto total)
--
-- Restricciones:
--   - el periodo debe existir en los eventos
--------------------------------------------------------------------------------
calcularResumenDePeriodo :: [Evento] -> (Int, Int) -> (String, Int, Float)
calcularResumenDePeriodo eventos (anio, mes) =
    let 
        eventosDelPeriodo = filtrarEventosPorPeriodo eventos anio mes
        cantidadEventos    = length eventosDelPeriodo
        montoTotalPeriodo  = calcularMontoTotal eventosDelPeriodo
        nombrePeriodo      = construirNombrePeriodo anio mes
    in 
        (nombrePeriodo, cantidadEventos, montoTotalPeriodo)


--------------------------------------------------------------------------------
-- Nombre: filtrarEventosPorPeriodo
--
-- Objetivo: filtra eventos por año y mes
--
-- Entradas:
--   - lista de eventos
--   - año
--   - mes
--
-- Salida: eventos que coinciden con ese periodo
--
-- Restricciones:
--   - depende de que los timestamps sean válidos
--------------------------------------------------------------------------------
filtrarEventosPorPeriodo :: [Evento] -> Int -> Int -> [Evento]
filtrarEventosPorPeriodo eventos anio mes =
    let coincidePeriodo evento =
            extraerAnio evento == anio &&
            extraerMes evento == mes
    in filter coincidePeriodo eventos