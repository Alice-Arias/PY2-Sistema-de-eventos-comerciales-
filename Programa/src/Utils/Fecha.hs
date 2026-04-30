module Utils.Fecha
(
    -- nuevos nombres
    obtenerMes,
    obtenerAnio,
    formatearMesAno,
    formatearFecha,
    nombreMes,

    -- compatibilidad con código viejo (NO rompe nada)
    extraerMes,
    extraerAnio,
    extraerMesAno
) where

import Types.Evento
import Data.Time


-- =========================
-- CONVERSIÓN DE FECHA
-- =========================

timestampADate :: Int -> Day
timestampADate t =
    let anio = t `div` 10000
        mes  = (t `div` 100) `mod` 100
        dia  = t `mod` 100
    in fromGregorian (toInteger anio) mes dia


-- =========================
-- NUEVO ESTÁNDAR
-- =========================

obtenerMes :: Evento -> Int
obtenerMes evento =
    let (_, mes, _) = toGregorian (timestampADate (timestamp evento))
    in mes


obtenerAnio :: Evento -> Integer
obtenerAnio evento =
    let (anio, _, _) = toGregorian (timestampADate (timestamp evento))
    in anio


formatearMesAno :: Int -> String
formatearMesAno fecha =
    let mes  = (fecha `div` 100) `mod` 100
        anio = fecha `div` 10000
    in nombreMes mes ++ " " ++ show anio


formatearFecha :: Int -> String
formatearFecha fecha =
    let dia  = fecha `mod` 100
        mes  = (fecha `div` 100) `mod` 100
        anio = fecha `div` 10000
    in show dia ++ " " ++ nombreMes mes ++ " " ++ show anio


-- =========================
-- MESES
-- =========================

nombreMes :: Int -> String
nombreMes 1  = "Enero"
nombreMes 2  = "Febrero"
nombreMes 3  = "Marzo"
nombreMes 4  = "Abril"
nombreMes 5  = "Mayo"
nombreMes 6  = "Junio"
nombreMes 7  = "Julio"
nombreMes 8  = "Agosto"
nombreMes 9  = "Septiembre"
nombreMes 10 = "Octubre"
nombreMes 11 = "Noviembre"
nombreMes 12 = "Diciembre"
nombreMes _  = "Desconocido"


-- =========================
-- COMPATIBILIDAD (IMPORTANTE)
-- =========================

extraerMes :: Evento -> Int
extraerMes = obtenerMes


extraerAnio :: Evento -> Integer
extraerAnio = obtenerAnio


extraerMesAno :: Int -> String
extraerMesAno = formatearMesAno