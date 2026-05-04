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
--
-- Objetivo: muestra el mes con mayor ingreso total y el día con más actividad
--
-- Entradas: lista de eventos del sistema
--
-- Salida: IO () que imprime resultados en pantalla
--
-- Restricciones:
--   - La lista de eventos no debe estar vacía para resultados representativos
--   - Depende de funciones de análisis en otros módulos
--------------------------------------------------------------------------------
analisisMesDia :: [Evento] -> IO ()
analisisMesDia eventos = do

    let (mesConMasDinero, totalDelMes) = obtenerMesMayor eventos

    let (diaConMasActividad, totalDelDia) = DS.diaMasActivo eventos

    mostrarMesDiaUI mesConMasDinero totalDelMes diaConMasActividad totalDelDia

--------------------------------------------------------------------------------
-- Nombre: obtenerMesMayor
--
-- Objetivo: obtiene el mes con mayor acumulación de dinero
--
-- Entradas: lista de eventos
--
-- Salida: par (mes, monto total)
--
-- Restricciones:
--   - Si la lista está vacía, el resultado no es significativo
--------------------------------------------------------------------------------
obtenerMesMayor :: [Evento] -> (String, Float)
obtenerMesMayor = mesConMayorMonto

--------------------------------------------------------------------------------
-- Nombre: analisisExtremos
--
-- Objetivo: muestra el evento más antiguo y el más reciente del sistema
--
-- Entradas: lista de eventos
--
-- Salida: IO () con salida en pantalla
--
-- Restricciones:
--   - La lista no debe estar vacía
--------------------------------------------------------------------------------
analisisExtremos :: [Evento] -> IO ()
analisisExtremos eventos = do

    let (eventoMasAntiguo, eventoMasNuevo) = obtenerExtremos eventos

    mostrarExtremosUI eventoMasAntiguo eventoMasNuevo

--------------------------------------------------------------------------------
-- Nombre: obtenerExtremos
--
-- Objetivo: obtiene el evento más antiguo y el más reciente
--
-- Entradas: lista de eventos
--
-- Salida: par (evento viejo, evento nuevo)
--
-- Restricciones:
--   - La lista no debe estar vacía
--------------------------------------------------------------------------------
obtenerExtremos :: [Evento] -> (Evento, Evento)
obtenerExtremos = eventosExtremos

--------------------------------------------------------------------------------
-- Nombre: analisisResumen
--
-- Objetivo: agrupa eventos por intervalos de tiempo y muestra un resumen
--
-- Entradas: lista de eventos
--
-- Salida: IO () con reporte en pantalla
--
-- Restricciones:
--   - Si la lista está vacía se muestra un mensaje de error
--   - Depende de funciones de agrupación temporal
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
--
-- Objetivo: obtiene la fecha más antigua y la más reciente del sistema
--
-- Entradas: lista de eventos
--
-- Salida: (fecha inicio, fecha fin)
--
-- Restricciones:
--   - La lista no debe estar vacía
--------------------------------------------------------------------------------
obtenerRangoFechas :: [Evento] -> (Day, Day)
obtenerRangoFechas eventos =
    let (eventoViejo, eventoNuevo) = eventosExtremos eventos

    in (intToDay (timestamp eventoViejo), intToDay (timestamp eventoNuevo))

--------------------------------------------------------------------------------
-- Nombre: calcularDiasDisponibles
--
-- Objetivo: calcula la cantidad de días entre dos fechas
--
-- Entradas: fecha inicial y fecha final
--
-- Salida: número de días entre ambas fechas
--
-- Restricciones:
--   - La fecha inicial debe ser menor o igual a la final
--------------------------------------------------------------------------------
calcularDiasDisponibles :: Day -> Day -> Int
calcularDiasDisponibles inicio fin = fromIntegral (diffDays fin inicio)

--------------------------------------------------------------------------------
-- Nombre: agruparPorIntervalo
--
-- Objetivo: divide los eventos en grupos de fechas según un intervalo dado
--
-- Entradas: tamaño del intervalo en días y lista de eventos
--
-- Salida: lista de grupos (inicio, fin, eventos)
--
-- Restricciones:
--   - La lista de eventos debe estar ordenada por fecha
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
--
-- Objetivo: construye grupos de eventos dentro de rangos de fechas
--
-- Entradas: fecha actual, fecha final, tamaño de grupo y lista ordenada de eventos
--
-- Salida: lista de grupos de eventos por rango de fechas
--
-- Restricciones:
--   - La lista debe estar ordenada por fecha
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