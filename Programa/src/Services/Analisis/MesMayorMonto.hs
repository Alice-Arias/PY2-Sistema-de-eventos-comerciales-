module Services.Analisis.MesMayorMonto where

import Types.Modelos
import Types.Fecha
import Data.List 
import Data.Ord 

--------------------------------------------------------------------------------
-- Nombre: obtenerMesDelEvento
-- Entrada: un evento del sistema
-- Salida: nombre del mes y año donde ocurrió ese evento
-- Restricciones:
--   - El evento debe tener una fecha válida
--------------------------------------------------------------------------------
obtenerMesDelEvento :: Evento -> String
obtenerMesDelEvento evento = extraerMesAno (timestamp evento)


--------------------------------------------------------------------------------
-- Nombre: obtenerMesesUnicos
-- Entrada: lista de eventos del sistema
-- Salida: lista de meses sin repetir donde ocurrieron eventos
-- Restricciones:
--   - Puede devolver lista vacía si no hay eventos
--------------------------------------------------------------------------------
obtenerMesesUnicos :: [Evento] -> [String]
obtenerMesesUnicos eventos = nub (map obtenerMesDelEvento eventos)


--------------------------------------------------------------------------------
-- Nombre: filtrarEventosPorMes
-- Entrada:
--   mes: mes específico en formato texto
--   eventos: lista de eventos del sistema
-- Salida: eventos que ocurrieron en ese mes
-- Restricciones:
--   - El mes debe coincidir con el formato generado por el sistema
--------------------------------------------------------------------------------
filtrarEventosPorMes :: String -> [Evento] -> [Evento]
filtrarEventosPorMes mes = filter (\evento -> obtenerMesDelEvento evento == mes)


--------------------------------------------------------------------------------
-- Nombre: calcularTotalDelMes
-- Entrada:
--   mes: mes a analizar
--   eventos: lista de eventos del sistema
-- Salida: suma total de dinero generado en ese mes
-- Restricciones:
--   - Si no hay eventos en el mes, devuelve 0
--------------------------------------------------------------------------------
calcularTotalDelMes :: String -> [Evento] -> Float
calcularTotalDelMes mes eventos =
    let 
        eventosDelMes = filtrarEventosPorMes mes eventos
    in 
        sum (map total eventosDelMes)


--------------------------------------------------------------------------------
-- Nombre: construirResumenDelMes
-- Entrada:
--   mes: mes a analizar
--   eventos: lista de eventos del sistema
-- Salida: par con el mes y su total generado
-- Restricciones:
--   - Ninguna
--------------------------------------------------------------------------------
construirResumenDelMes :: String -> [Evento] -> (String, Float)
construirResumenDelMes mes eventos =

    let totalMes = calcularTotalDelMes mes eventos

    in (mes, totalMes)


--------------------------------------------------------------------------------
-- Nombre: construirResumenMensual
-- Entrada:
--   meses: lista de meses únicos
--   eventos: lista de eventos del sistema
-- Salida: lista de (mes, total generado)
-- Restricciones:
--   - Cada mes se analiza una sola vez
--------------------------------------------------------------------------------
construirResumenMensual :: [String] -> [Evento] -> [(String, Float)]
construirResumenMensual meses eventos = map (\mes -> construirResumenDelMes mes eventos) meses


--------------------------------------------------------------------------------
-- Nombre: mesConMayorMonto
-- Entrada: lista de eventos del sistema
-- Salida:
--   mes con mayor dinero generado y el monto total de ese mes
-- Restricciones:
--   - La lista no debe estar vacía para obtener resultado correcto
--------------------------------------------------------------------------------
mesConMayorMonto :: [Evento] -> (String, Float)
mesConMayorMonto eventos =

    let 
        mesesUnicos = obtenerMesesUnicos eventos
        resumenPorMes = construirResumenMensual mesesUnicos eventos
    in 
        maximumBy (comparing snd) resumenPorMes