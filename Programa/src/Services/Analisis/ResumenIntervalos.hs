module Services.Analisis.ResumenIntervalos where

import Types.Evento
import Utils.Fecha (extraerAnio, extraerMes, nombreMes)
import Data.List (nub, sortOn)


resumenIntervalos :: [Evento] -> [(String, Int, Float)]
resumenIntervalos listaEventos =
    let periodos = obtenerPeriodosOrdenados listaEventos
    in map (calcularResumen listaEventos) periodos

obtenerPeriodosOrdenados :: [Evento] -> [(Int, Int)]
obtenerPeriodosOrdenados eventos =
    let periodosUnicos = obtenerPeriodosUnicos eventos
    in sortOn (\(anio, mes) -> (anio, mes)) periodosUnicos

obtenerPeriodosUnicos :: [Evento] -> [(Int, Int)]
obtenerPeriodosUnicos eventos = nub [ (extraerAnio evento, extraerMes evento) | evento <- eventos ]

calcularResumen :: [Evento] -> (Int, Int) -> (String, Int, Float)
calcularResumen eventos (anio, mes) =
    let eventosDelPeriodo = filtrarEventosPorPeriodo eventos anio mes
        cantidadEventos   = length eventosDelPeriodo
        montoTotal        = calcularMontoTotal eventosDelPeriodo
        nombrePeriodo     = construirNombrePeriodo anio mes
    in (nombrePeriodo, cantidadEventos, montoTotal)


filtrarEventosPorPeriodo :: [Evento] -> Int -> Int -> [Evento]
filtrarEventosPorPeriodo eventos anio mes =
    filter (\evento -> extraerAnio evento == anio &&extraerMes evento == mes) eventos


-- Suma el total de eventos
calcularMontoTotal :: [Evento] -> Float
calcularMontoTotal eventos = sum (map total eventos)


-- Construye el nombre del periodo
construirNombrePeriodo :: Int -> Int -> String
construirNombrePeriodo anio mes = nombreMes mes ++ " " ++ show anio