module Services.Analisis.ResumenIntervalos where

import Types.Modelos
import Types.Fecha
import Data.List (nub, sortOn)
import Utils.Formato
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: resumenIntervalos
-- Entrada: lista de eventos del sistema
-- Salida:
--   Lista con resumen por cada mes y año:
--   - nombre del periodo (mes y año)
--   - cantidad de eventos
--   - monto total generado
-- Restricciones:
--   - Los eventos deben tener fecha válida
--------------------------------------------------------------------------------
resumenIntervalos :: [Evento] -> [(String, Int, Float)]
resumenIntervalos eventos =
    let 
        periodosOrdenados = obtenerPeriodosOrdenados eventos
    in 
        map (calcularResumenDePeriodo eventos) periodosOrdenados


--------------------------------------------------------------------------------
-- Nombre: obtenerPeriodosOrdenados
-- Entrada: lista de eventos del sistema
-- Salida:
--   lista de (año, mes) sin repetir, ordenada del más antiguo al más reciente
-- Restricciones:
--   - depende de que los eventos tengan fecha válida
--------------------------------------------------------------------------------
obtenerPeriodosOrdenados :: [Evento] -> [(Int, Int)]
obtenerPeriodosOrdenados eventos =
    let 
        periodosSinRepetir = obtenerPeriodosUnicos eventos
    in 
        sortOn (\(anio, mes) -> (anio, mes)) periodosSinRepetir


--------------------------------------------------------------------------------
-- Nombre: obtenerPeriodosUnicos
-- Entrada: lista de eventos del sistema
-- Salida:
--   lista de (año, mes) sin repetir
-- Restricciones:
--   - elimina duplicados automáticamente
--------------------------------------------------------------------------------
obtenerPeriodosUnicos :: [Evento] -> [(Int, Int)]
obtenerPeriodosUnicos eventos = nub [ (extraerAnio evento, extraerMes evento) | evento <- eventos ]


--------------------------------------------------------------------------------
-- Nombre: calcularResumenDePeriodo
-- Entrada:
--   lista de eventos del sistema
--   un periodo (año, mes)
-- Salida:
--   resumen del periodo:
--   - nombre del mes y año
--   - cantidad de eventos
--   - dinero total generado
-- Restricciones:
--   - el periodo debe existir dentro de los eventos
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
-- Entrada:
--   lista de eventos
--   año y mes específico
-- Salida:
--   eventos que pertenecen a ese mes y año
-- Restricciones:
--   - comparación basada en fecha del evento
--------------------------------------------------------------------------------
filtrarEventosPorPeriodo :: [Evento] -> Int -> Int -> [Evento]
filtrarEventosPorPeriodo eventos anio mes = filter (\evento -> extraerAnio evento == anio && extraerMes evento == mes) eventos