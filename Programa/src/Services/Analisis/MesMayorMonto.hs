module Services.Analisis.MesMayorMonto where

import Types.Evento
import Utils.Fecha (extraerMesAno)
import Data.List (nub, maximumBy)
import Data.Ord (comparing)

obtenerMesEvento :: Evento -> String
obtenerMesEvento evento = extraerMesAno (timestamp evento)

obtenerMesesUnicos :: [Evento] -> [String]
obtenerMesesUnicos eventos = nub (map obtenerMesEvento eventos)


filtrarEventosPorMes :: String -> [Evento] -> [Evento]
filtrarEventosPorMes mes = filter (\evento -> obtenerMesEvento evento == mes)

calcularTotalMes :: String -> [Evento] -> Float
calcularTotalMes mes eventos =
    let eventosDelMes = filtrarEventosPorMes mes eventos
    in sum (map total eventosDelMes)


construirResumenMes :: String -> [Evento] -> (String, Float)
construirResumenMes mes eventos =
    let totalMes = calcularTotalMes mes eventos
    in (mes, totalMes)


construirResumenMensual :: [String] -> [Evento] -> [(String, Float)]
construirResumenMensual meses eventos =
    map (\mes -> construirResumenMes mes eventos) meses



mesConMayorMonto :: [Evento] -> (String, Float)
mesConMayorMonto eventos =
    let
        mesesUnicos = obtenerMesesUnicos eventos

        resumenPorMes = construirResumenMensual mesesUnicos eventos

    in
        maximumBy (comparing snd) resumenPorMes