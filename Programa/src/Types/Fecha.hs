module Types.Fecha where

import Data.Time
import Text.Printf
import Data.Char
import Types.Modelos
import Utils.Formato
import Data.List
import Data.Function

--------------------------------------------------------------------------------
-- extraerAnioDeFecha
-- Entrada: número de fecha en formato YYYYMMDD
-- Salida: año de la fecha
-- Restricciones: la fecha debe estar en formato válido
--------------------------------------------------------------------------------
extraerAnio :: Int -> Integer
extraerAnio fechaCompleta = fromIntegral (fechaCompleta `div` 10000)

--------------------------------------------------------------------------------
-- extraerMesDeFecha
-- Entrada: número de fecha en formato YYYYMMDD
-- Salida: mes de la fecha
-- Restricciones: la fecha debe estar en formato válido
--------------------------------------------------------------------------------
extraerMes :: Int -> Int
extraerMes fechaCompleta = (fechaCompleta `div` 100) `mod` 100

--------------------------------------------------------------------------------
--  extraerDiaDeFecha
-- Entrada: número de fecha en formato YYYYMMDD
-- Salida: día de la fecha
-- Restricciones: la fecha debe estar en formato válido
--------------------------------------------------------------------------------
extraerDia :: Int -> Int
extraerDia fechaCompleta = fechaCompleta `mod` 100
--------------------------------------------------------------------------------
--  convertirNumeroAFecha
-- Entrada: número de fecha en formato YYYYMMDD
-- Salida: fecha tipo Day del sistema
-- Restricciones: el número debe representar una fecha válida
--------------------------------------------------------------------------------
intToDay :: Int -> Day
intToDay fecha =
    let
        anio = extraerAnio fecha
        mes  = extraerMes fecha
        dia  = extraerDia fecha
    in
        fromGregorian anio mes dia

--------------------------------------------------------------------------------
--  obtenerAnioDeFecha
-- Entrada: fecha tipo Day
-- Salida: año de la fecha
-- Restricciones: ninguna
--------------------------------------------------------------------------------
obtenerAnio :: Day -> Integer
obtenerAnio fechaCompleta =
    let (anio, _, _) = toGregorian fechaCompleta  -- toGregorian devuelve (año, mes, día)
    in anio

--------------------------------------------------------------------------------
-- obtenerMesDeFecha
-- Entrada: fecha tipo Day
-- Salida: mes de la fecha
-- Restricciones: ninguna
--------------------------------------------------------------------------------
obtenerMes :: Day -> Int
obtenerMes fechaCompleta =
    let (_, mes, _) = toGregorian fechaCompleta
    in mes

--------------------------------------------------------------------------------
--  convertirMesYAnioATexto
-- Entrada: número de fecha en formato YYYYMMDD
-- Salida: texto con mes y año (ej: "Mayo 2026")
-- Restricciones: la fecha debe ser válida
--------------------------------------------------------------------------------
extraerMesAno :: Int -> String
extraerMesAno = formatearMesAno

--------------------------------------------------------------------------------
-- obtenerDiaDeFecha
-- Entrada: fecha tipo Day
-- Salida: día de la fecha
-- Restricciones: ninguna
--------------------------------------------------------------------------------
obtenerDia :: Day -> Int
obtenerDia fechaCompleta =
    let (_, _, dia) = toGregorian fechaCompleta
    in dia

--------------------------------------------------------------------------------
-- convertirFechaAEntero
-- Entrada: año, mes y día
-- Salida: número en formato YYYYMMDD
-- Restricciones: valores deben ser válidos
--------------------------------------------------------------------------------
formatearComoInt :: Integer -> Int -> Int -> Int
formatearComoInt anioValor mesValor diaValor =
    read (printf "%04d%02d%02d" anioValor mesValor diaValor)
--------------------------------------------------------------------------------
--  convertirFechaSistemaAEntero
-- Entrada: fecha tipo Day
-- Salida: fecha en formato YYYYMMDD
-- Restricciones: ninguna
--------------------------------------------------------------------------------
dayToInt :: Day -> Int
dayToInt fecha =
    let anio = obtenerAnio fecha
        mes  = obtenerMes fecha
        dia  = obtenerDia fecha
    in formatearComoInt anio mes dia

--------------------------------------------------------------------------------
--  obtenerNombreMes
-- Entrada: número del mes
-- Salida: nombre del mes en texto
-- Restricciones: si el número no es válido devuelve "Desconocido"
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- formatearFechaCompleta
-- Entrada: número de fecha en formato YYYYMMDD
-- Salida: fecha en texto legible
-- Restricciones: la fecha debe ser válida
--------------------------------------------------------------------------------
formatearFecha :: Int -> String
formatearFecha fecha =
    let
        dia  = fecha `mod` 100
        mes  = (fecha `div` 100) `mod` 100
        anio = fecha `div` 10000
    in
        show dia ++ " " ++ nombreMes mes ++ " " ++ show anio

--------------------------------------------------------------------------------
-- formatearMesYAnio
-- Entrada: número de fecha en formato YYYYMMDD
-- Salida: texto con mes y año
-- Restricciones: la fecha debe ser válida
--------------------------------------------------------------------------------
formatearMesAno :: Int -> String
formatearMesAno fecha =
    let
        mes  = (fecha `div` 100) `mod` 100
        anio = fecha `div` 10000
    in
        nombreMes mes ++ " " ++ show anio
--------------------------------------------------------------------------------
-- validarFechaTexto
-- Entrada: texto con formato dd-mm-yyyy
-- Salida: fecha en formato YYYYMMDD si es válida
-- Restricciones: debe cumplir formato correcto y valores válidos
--------------------------------------------------------------------------------
parseFecha :: String -> Maybe Int
parseFecha textoFecha =
    case split '-' textoFecha of
        [diaTexto, mesTexto, anioTexto]
            | esFechaValida diaTexto mesTexto anioTexto ->
                Just (toInt anioTexto * 10000 + toInt mesTexto * 100 + toInt diaTexto)
        _ -> Nothing

--------------------------------------------------------------------------------
--  validarPartesDeFecha
-- Entrada: día, mes y año como texto
-- Salida: True si la fecha es válida
-- Restricciones: solo acepta números y rangos correctos
--------------------------------------------------------------------------------
esFechaValida :: String -> String -> String -> Bool
esFechaValida dd mm yyyy =
    soloDigitos dd &&
    soloDigitos mm &&
    soloDigitos yyyy &&

    not (null dd || null mm || null yyyy) &&
    length yyyy == 4 &&

    dentroRango (toInt dd) 1 31 &&
    dentroRango (toInt mm) 1 12

--------------------------------------------------------------------------------
--  verificarSoloNumeros
-- Entrada: texto
-- Salida: True si solo contiene números
-- Restricciones: ninguna
--------------------------------------------------------------------------------
soloDigitos :: String -> Bool
soloDigitos = all isDigit

--------------------------------------------------------------------------------
--  verificarRango
-- Entrada: número, mínimo y máximo
-- Salida: True si está dentro del rango
-- Restricciones: min debe ser menor o igual a max
--------------------------------------------------------------------------------
dentroRango :: Int -> Int -> Int -> Bool
dentroRango valorActual valorMinimo valorMaximo =
    valorActual >= valorMinimo && valorActual <= valorMaximo
--------------------------------------------------------------------------------
--  convertirTextoANumero
-- Entrada: texto numérico
-- Salida: número entero
-- Restricciones: el texto debe ser convertible a número
--------------------------------------------------------------------------------
toInt :: String -> Int
toInt = read

--------------------------------------------------------------------------------
-- dividirTextoPorSeparador
-- Entrada: carácter separador y texto
-- Salida: lista de partes del texto
-- Restricciones: ninguna
--------------------------------------------------------------------------------
split :: Char -> String -> [String]
split separador texto = procesar texto ""

    where
    procesar [] acumulador = [reverse acumulador]

    procesar (caracter:resto) acumulador

        | caracter == separador = reverse acumulador : procesar resto ""

        | otherwise = procesar resto (caracter : acumulador)

--------------------------------------------------------------------------------
-- Nombre: filtrarEventosPorRangoDeFechas
-- Entrada: lista de eventos, fecha de inicio y fecha final (formato YYYYMMDD)
-- Salida: lista de eventos que están dentro del rango de fechas
-- Restricciones: las fechas deben ser válidas en formato entero YYYYMMDD
--------------------------------------------------------------------------------
filtrarEventosEnRango :: [Evento] -> Int -> Int -> [Evento]
filtrarEventosEnRango listaEventos fechaInicio fechaFin = filter (\evento -> timestamp evento >= fechaInicio &&  timestamp evento <= fechaFin  ) listaEventos

--------------------------------------------------------------------------------
-- Nombre: ordenarEventosPorFecha
-- Entrada: lista de eventos sin orden
-- Salida: lista de eventos ordenada desde el más antiguo al más reciente
-- Restricciones: ninguna
--------------------------------------------------------------------------------
ordenarEventosPorFecha :: [Evento] -> [Evento]
ordenarEventosPorFecha = sortBy (\eventoIzquierdo eventoDerecho ->
        compare (timestamp eventoIzquierdo) (timestamp eventoDerecho)
    )
--------------------------------------------------------------------------------
-- Nombre: ordenarFechas
-- Entrada: dos fechas enteras
-- Salida: una tupla (fechaMenor, fechaMayor)
-- Restricciones: ninguna
--------------------------------------------------------------------------------
ordenarFechas :: Int -> Int -> (Int, Int)
ordenarFechas fechaA fechaB
    | fechaA <= fechaB = (fechaA, fechaB)
    | otherwise    = (fechaB, fechaA)

--------------------------------------------------------------------------------
-- Nombre: construirNombrePeriodo
-- Entrada: año y mes
-- Salida: texto legible tipo "Mayo 2026"
-- Restricciones: el número de mes debe estar entre 1 y 12
--------------------------------------------------------------------------------
construirNombrePeriodo :: Int -> Int -> String
construirNombrePeriodo anio mes = nombreMes mes ++ " " ++ show anio