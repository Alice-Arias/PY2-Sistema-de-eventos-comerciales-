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
obtenerIndiceDia evento =
    timestamp evento `mod` 7


diaMasActivo :: [Evento] -> (String, Int)
diaMasActivo eventos =
    let

        -- lista de días únicos que aparecen en los eventos
        diasUnicos :: [Int]
        diasUnicos = nub (map obtenerIndiceDia eventos)

        -- cuenta cuántos eventos hay en un día específico
        contarEventosDelDia :: Int -> Int
        contarEventosDelDia dia =
            length (filter (\evento -> obtenerIndiceDia evento == dia) eventos)

        -- crea pares: (nombre del día, cantidad de eventos)
        resumenPorDia :: [(String, Int)]
        resumenPorDia =
            map (\dia ->
                (nombreDiaSemana dia, contarEventosDelDia dia)
            ) diasUnicos

    in
        maximumBy (\a b -> compare (snd a) (snd b)) resumenPorDia