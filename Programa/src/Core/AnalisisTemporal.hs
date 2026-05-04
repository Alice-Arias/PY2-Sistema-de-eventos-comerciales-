module Core.AnalisisTemporal where

import Services.Analisis.MesMayorMonto
import qualified Services.Analisis.DiaSemanaActivo as DS
import Services.Analisis.EventosExtremos

import UI.Interfaz
import Utils.Colores 
import Utils.Calculos

import Data.Time 
import Types.Modelos
import Types.Fecha 

--------------------------------------------------------------------------------
-- Nombre: analisisMesDia
-- Entrada: eventos: lista de registros del sistema donde se guardan todas las acciones realizadas
-- Salida: muestra en pantalla el mes con más dinero generado y el día con más actividad
-- Restricciones:
--   La lista de eventos no debe estar vacía para obtener resultados correctos
--   Depende de funciones de otros módulos de análisis
--------------------------------------------------------------------------------
analisisMesDia :: [Evento] -> IO ()
analisisMesDia eventos = do
    let (mesConMasDinero, totalDelMes) = obtenerMesMayor eventos
    let (diaConMasActividad, totalDelDia) = DS.diaMasActivo eventos

    mostrarMesDiaUI mesConMasDinero totalDelMes diaConMasActividad totalDelDia

--------------------------------------------------------------------------------
-- Nombre: obtenerMesMayor
-- Entrada: eventos: lista de registros del sistema donde se guardan todas las acciones realizadas
-- Salida: devuelve el nombre del mes con mayor dinero generado y el monto total
-- Restricciones:
--   Puede recibir lista vacía, pero el resultado no sería útil
--------------------------------------------------------------------------------
obtenerMesMayor :: [Evento] -> (String, Float)
obtenerMesMayor = mesConMayorMonto

--------------------------------------------------------------------------------
-- Nombre: analisisExtremos
-- Entrada: eventos: lista de registros del sistema donde se guardan todas las acciones realizadas
-- Salida: muestra en pantalla el evento más antiguo y el evento más reciente
-- Restricciones:
--   La lista no debe estar vacía
--------------------------------------------------------------------------------
analisisExtremos :: [Evento] -> IO ()
analisisExtremos eventos = do
    let (eventoMasAntiguo, eventoMasNuevo) = obtenerExtremos eventos
    mostrarExtremosUI eventoMasAntiguo eventoMasNuevo

--------------------------------------------------------------------------------
-- Nombre: obtenerExtremos
-- Entrada: eventos: lista de registros del sistema donde se guardan todas las acciones realizadas
-- Salida: devuelve el evento más antiguo y el evento más reciente
-- Restricciones:
--   La lista no debe estar vacía
--------------------------------------------------------------------------------
obtenerExtremos :: [Evento] -> (Evento, Evento)
obtenerExtremos = eventosExtremos

--------------------------------------------------------------------------------
-- Nombre: analisisResumen
-- Entrada: eventos: lista de registros del sistema donde se guardan todas las acciones realizadas
-- Salida: muestra un resumen de eventos agrupados por intervalos de tiempo
-- Restricciones:
--   Si la lista está vacía se muestra un mensaje de error
--   Depende de funciones de otros módulos para agrupar datos
--------------------------------------------------------------------------------
analisisResumen :: [Evento] -> IO ()
analisisResumen [] = putStrLn (errorMsg "No hay eventos.")
analisisResumen eventos = do

    let (fechaInicio, fechaFinal) = obtenerRangoFechas eventos
    let cantidadDias = calcularDiasDisponibles fechaInicio fechaFinal

    intervaloSeleccionado <- pedirIntervaloUI cantidadDias

    let gruposDeEventos = agruparPorIntervalo intervaloSeleccionado eventos
    imprimirResumenUI gruposDeEventos

--------------------------------------------------------------------------------
-- Nombre: obtenerRangoFechas
-- Entrada: eventos: lista de registros del sistema donde se guardan todas las acciones realizadas
-- Salida: devuelve la fecha más antigua y la fecha más reciente
-- Restricciones:
--   La lista no debe estar vacía
--------------------------------------------------------------------------------
obtenerRangoFechas :: [Evento] -> (Day, Day)
obtenerRangoFechas eventos =
    let (eventoViejo, eventoNuevo) = eventosExtremos eventos
    in (intToDay (timestamp eventoViejo), intToDay (timestamp eventoNuevo))

--------------------------------------------------------------------------------
-- Nombre: calcularDiasDisponibles
-- Entrada: fecha de inicio y fecha final
-- Salida: cantidad de días entre ambas fechas
-- Restricciones:
--   La fecha inicial debe ser menor o igual a la final
--------------------------------------------------------------------------------
calcularDiasDisponibles :: Day -> Day -> Int
calcularDiasDisponibles inicio fin = fromIntegral (diffDays fin inicio)

--------------------------------------------------------------------------------
-- Nombre: agruparPorIntervalo
-- Entrada:
--   diasPorGrupo: cantidad de días que tendrá cada grupo
--   eventos: lista de registros del sistema
-- Salida:
--   lista de grupos de eventos organizados por rango de fechas
-- Restricciones:
--   la lista de eventos debe estar ordenada por fecha
--------------------------------------------------------------------------------
agruparPorIntervalo :: Int -> [Evento] -> [(Day, Day, [Evento])]
agruparPorIntervalo _ [] = []
agruparPorIntervalo diasPorGrupo eventos =

    let eventosOrdenados = ordenarEventos eventos

        fechaInicio = obtenerFechaInicio eventosOrdenados
        fechaFin = obtenerFechaFin eventosOrdenados

    in agruparPorDias fechaInicio fechaFin diasPorGrupo eventosOrdenados

--------------------------------------------------------------------------------
-- Nombre: agruparPorDias
-- Entrada:
--   fechaActual: inicio del grupo
--   fechaFinal: límite total
--   diasPorGrupo: tamaño del intervalo
--   eventos: lista ordenada de eventos
-- Salida:
--   lista de grupos de eventos por rango de fechas
-- Restricciones:
--   la lista debe estar ordenada por fecha
--------------------------------------------------------------------------------
agruparPorDias :: Day -> Day -> Int -> [Evento] -> [(Day, Day, [Evento])]
agruparPorDias fechaActual fechaFinal diasPorGrupo eventos

    | fechaActual > fechaFinal = []

    | otherwise =
        let 
            limite = sumarDias fechaActual diasPorGrupo
            (grupoEventos, eventosRestantes) = separarEventos limite eventos
            limiteReal = ajustarFinIntervalo limite fechaFinal

        in 
            (fechaActual, limiteReal, grupoEventos) : agruparPorDias limite fechaFinal diasPorGrupo eventosRestantes