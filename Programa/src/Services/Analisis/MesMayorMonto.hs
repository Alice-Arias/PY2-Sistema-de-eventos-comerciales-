module Services.Analisis.MesMayorMonto where

import Types.Modelos
import Types.Fecha
import Data.List 
import Data.Ord 

--------------------------------------------------------------------------------
-- Nombre: obtenerMesDelEvento
--
-- Objetivo: obtiene el mes y año en el que ocurrió un evento
--
-- Entradas: evento del sistema
--
-- Salida: mes en formato texto (ej: "05-2026")
--
-- Restricciones:
--   - el evento debe tener una fecha válida en su timestamp
--------------------------------------------------------------------------------
obtenerMesDelEvento :: Evento -> String
obtenerMesDelEvento evento = extraerMesAno (timestamp evento)


--------------------------------------------------------------------------------
-- Nombre: obtenerMesesUnicos
--
-- Objetivo: obtiene todos los meses únicos en los que ocurrieron eventos
--
-- Entradas: lista de eventos
--
-- Salida: lista de meses sin repetición
--
-- Restricciones: puede devolver lista vacía si no hay eventos
--------------------------------------------------------------------------------
obtenerMesesUnicos :: [Evento] -> [String]
obtenerMesesUnicos eventos = nub (map obtenerMesDelEvento eventos)


--------------------------------------------------------------------------------
-- Nombre: filtrarEventosPorMes
--
-- Objetivo: filtra eventos que pertenecen a un mes específico
--
-- Entradas:
--   - mes en formato texto
--   - lista de eventos
--
-- Salida: lista de eventos del mes indicado
--
-- Restricciones:
--   - el formato del mes debe coincidir con el generado por el sistema
--------------------------------------------------------------------------------
filtrarEventosPorMes :: String -> [Evento] -> [Evento]
filtrarEventosPorMes mes =

    let esDelMes evento = obtenerMesDelEvento evento == mes

    in filter esDelMes

--------------------------------------------------------------------------------
-- Nombre: calcularTotalDelMes
--
-- Objetivo: calcula el total de dinero generado en un mes específico
--
-- Entradas:
--   - mes a analizar
--   - lista de eventos
--
-- Salida: suma total de los valores del mes
--
-- Restricciones:
--   - si no hay eventos en el mes, el resultado es 0
--------------------------------------------------------------------------------
calcularTotalDelMes :: String -> [Evento] -> Float
calcularTotalDelMes mes eventos =
    let 
        eventosDelMes = filtrarEventosPorMes mes eventos
    in 
        sum (map total eventosDelMes)


--------------------------------------------------------------------------------
-- Nombre: construirResumenDelMes
--
-- Objetivo: construye el total generado para un mes específico
--
-- Entradas:
--   - mes
--   - lista de eventos
--
-- Salida: par (mes, total generado)
--
-- Restricciones: ninguna
--------------------------------------------------------------------------------
construirResumenDelMes :: String -> [Evento] -> (String, Float)
construirResumenDelMes mes eventos =

    let totalMes = calcularTotalDelMes mes eventos

    in (mes, totalMes)


--------------------------------------------------------------------------------
-- Nombre: construirResumenMensual
--
-- Objetivo: genera un resumen de ingresos por cada mes único
--
-- Entradas:
--   - lista de meses únicos
--   - lista de eventos
--
-- Salida: lista de pares (mes, total)
--
-- Restricciones:
--   - cada mes se evalúa una sola vez
--------------------------------------------------------------------------------
construirResumenMensual :: [String] -> [Evento] -> [(String, Float)]
construirResumenMensual listaMeses eventosSistema =

    let resumenMes mes = construirResumenDelMes mes eventosSistema

    in map resumenMes listaMeses

--------------------------------------------------------------------------------
-- Nombre: mesConMayorMonto
--
-- Objetivo: identifica el mes con mayor generación de dinero
--
-- Entradas: lista de eventos
--
-- Salida: par (mes, monto total)
--
-- Restricciones:
--   - la lista no debe estar vacía para obtener un resultado válido
--------------------------------------------------------------------------------
mesConMayorMonto :: [Evento] -> (String, Float)
mesConMayorMonto eventos =

    let 
        mesesUnicos = obtenerMesesUnicos eventos
        resumenPorMes = construirResumenMensual mesesUnicos eventos
    in 
        maximumBy (comparing snd) resumenPorMes