module Services.Analisis.DiaSemanaActivo where

import Types.Evento
import Data.List (nub, maximumBy)

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

obtenerIndiceDia :: Evento -> Int
obtenerIndiceDia evento = timestamp evento `mod` 7

obtenerDiasUnicos :: [Evento] -> [Int]
obtenerDiasUnicos eventos = nub (map obtenerIndiceDia eventos)


filtrarEventosPorDia :: Int -> [Evento] -> [Evento]
filtrarEventosPorDia dia = filter (\evento -> obtenerIndiceDia evento == dia)

contarEventosPorDia :: Int -> [Evento] -> Int
contarEventosPorDia dia eventos = length (filtrarEventosPorDia dia eventos)


construirResumenDia :: Int -> [Evento] -> (String, Int)
construirResumenDia dia eventos =
    let
        nombreDia = nombreDiaSemana dia
        cantidadEventos = contarEventosPorDia dia eventos
    in
        (nombreDia, cantidadEventos)


construirResumenGeneral :: [Int] -> [Evento] -> [(String, Int)]
construirResumenGeneral dias eventos =
    map (\dia -> construirResumenDia dia eventos) dias


diaMasActivo :: [Evento] -> (String, Int)
diaMasActivo eventos =
    let
        diasUnicos = obtenerDiasUnicos eventos

        resumenPorDia = construirResumenGeneral diasUnicos eventos

    in
        maximumBy (\a b -> compare (snd a) (snd b)) resumenPorDia