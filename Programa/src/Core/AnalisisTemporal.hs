module Core.AnalisisTemporal where

import Types.Evento
import Services.Analisis.MesMayorMonto
import Services.Analisis.DiaSemanaActivo
import Services.Analisis.EventosExtremos
import UI.AnalisisTemporalUI
import Utils.Colores (errorMsg)
import Data.Time (Day, addDays, diffDays)
import Types.Fecha (intToDay)
import Data.List (sortOn)


analisisMesDia :: [Evento] -> IO ()
analisisMesDia eventos = do
    let (mesTop, montoMes) = obtenerMesMayor eventos
    let (diaTop, totalDia) = obtenerDiaMasActivo eventos

    mostrarMesDiaUI mesTop montoMes diaTop totalDia

obtenerMesMayor :: [Evento] -> (String, Float)
obtenerMesMayor = mesConMayorMonto

obtenerDiaMasActivo :: [Evento] -> (String, Int)
obtenerDiaMasActivo = diaMasActivo


analisisExtremos :: [Evento] -> IO ()
analisisExtremos eventos = do
    let (eventoViejo, eventoNuevo) = obtenerExtremos eventos
    mostrarExtremosUI eventoViejo eventoNuevo

obtenerExtremos :: [Evento] -> (Evento, Evento)
obtenerExtremos = eventosExtremos

analisisResumen :: [Evento] -> IO ()
analisisResumen [] = putStrLn (errorMsg "No hay eventos.")
analisisResumen eventos = do

    let (fechaInicio, fechaFin) = obtenerRangoFechas eventos
    let diasDisponibles = calcularDiasDisponibles fechaInicio fechaFin

    intervalo <- pedirIntervaloUI diasDisponibles

    let grupos = agruparPorIntervalo intervalo eventos

    imprimirResumenUI grupos


obtenerRangoFechas :: [Evento] -> (Day, Day)
obtenerRangoFechas eventos =
    let (eventoViejo, eventoNuevo) = eventosExtremos eventos
    in ( intToDay (timestamp eventoViejo) , intToDay (timestamp eventoNuevo))

calcularDiasDisponibles :: Day -> Day -> Int
calcularDiasDisponibles inicio fin = fromIntegral (diffDays fin inicio)

agruparPorIntervalo :: Int -> [Evento] -> [(Day, Day, [Evento])]
agruparPorIntervalo _ [] = []
agruparPorIntervalo dias eventos =
    let 
        eventosOrdenados = ordenarEventos eventos
        fechaInicio = obtenerFechaInicio eventosOrdenados
        fechaFin    = obtenerFechaFin eventosOrdenados

    in agruparPorDias fechaInicio fechaFin dias eventosOrdenados

ordenarEventos :: [Evento] -> [Evento]
ordenarEventos = sortOn timestamp

obtenerFechaInicio :: [Evento] -> Day
obtenerFechaInicio eventos = intToDay (timestamp (head eventos))

obtenerFechaFin :: [Evento] -> Day
obtenerFechaFin eventos = intToDay (timestamp (last eventos))

agruparPorDias :: Day -> Day -> Int -> [Evento] -> [(Day, Day, [Evento])]
agruparPorDias fechaActual fechaFinal dias eventos
    | fechaActual > fechaFinal = []
    | otherwise =
        let fechaLimite = sumarDias fechaActual dias

            (grupoActual, restoEventos) = separarEventos fechaLimite eventos

            fechaFinIntervalo = ajustarFinIntervalo fechaLimite fechaFinal

        in (fechaActual, fechaFinIntervalo, grupoActual) : agruparPorDias fechaLimite fechaFinal dias restoEventos

sumarDias :: Day -> Int -> Day
sumarDias fecha dias = addDays (fromIntegral dias) fecha

separarEventos :: Day -> [Evento] -> ([Evento], [Evento])
separarEventos fechaLimite = span (\e -> intToDay (timestamp e) < fechaLimite)

ajustarFinIntervalo :: Day -> Day -> Day
ajustarFinIntervalo fechaLimite = min (addDays (-1) fechaLimite)