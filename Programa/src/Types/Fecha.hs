module Types.Fecha where

import Data.Time
import Text.Printf


extraerAnio :: Int -> Integer
extraerAnio fecha = fromIntegral (fecha `div` 10000)


extraerMes :: Int -> Int
extraerMes fecha = (fecha `div` 100) `mod` 100


extraerDia :: Int -> Int
extraerDia fecha =
    fecha `mod` 100


intToDay :: Int -> Day
intToDay fecha =
    let anio = extraerAnio fecha
        mes  = extraerMes fecha
        dia  = extraerDia fecha
    in
        fromGregorian anio mes dia



obtenerAnio :: Day -> Integer
obtenerAnio fecha =
    let (y, _, _) = toGregorian fecha
    in y


obtenerMes :: Day -> Int
obtenerMes fecha =
    let (_, m, _) = toGregorian fecha
    in m


obtenerDia :: Day -> Int
obtenerDia fecha =
    let (_, _, d) = toGregorian fecha
    in d


formatearComoInt :: Integer -> Int -> Int -> Int
formatearComoInt anio mes dia =
    read (printf "%04d%02d%02d" anio mes dia)


dayToInt :: Day -> Int
dayToInt fecha =
    let anio = obtenerAnio fecha
        mes  = obtenerMes fecha
        dia  = obtenerDia fecha
    in
        formatearComoInt anio mes dia