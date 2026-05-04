module Services.Estadisticas where

import Types.Modelos
import Types.Fecha
import Data.List
import Data.Function
import UI.Interfaz
import Utils.Colores
import Utils.Calculos
import Utils.Formato

--------------------------------------------------------------------------------
-- Nombre: generarEstadisticas
--
-- Objetivo: genera una estadística completa del sistema, la guarda en CSV
--           y la muestra en pantalla
--
-- Entradas: lista de eventos del sistema
--
-- Salida: IO Estadistica con la estructura generada
--
-- Restricciones:
--   - los eventos deben tener datos válidos
--   - depende de otros módulos de análisis y formato
--------------------------------------------------------------------------------
generarEstadisticas :: [Evento] -> IO Estadistica
generarEstadisticas eventos = do

    let
        estadisticaFinal = construirEstadistica eventos

    guardarCSV eventos estadisticaFinal

    putStrLn (okMsg "Estadística generada correctamente")
    mostrarEstadistica eventos estadisticaFinal

    return estadisticaFinal


--------------------------------------------------------------------------------
-- Nombre: construirEstadistica
--
-- Objetivo: construye la estructura final de estadísticas del sistema
--
-- Entradas: lista de eventos
--
-- Salida: estructura Estadistica con todos los datos calculados
--
-- Restricciones:
--   - los eventos deben contener información válida
--------------------------------------------------------------------------------
construirEstadistica :: [Evento] -> Estadistica
construirEstadistica eventos =
    let
        idEstadistica = generarId eventos
    in
        Estadistica
        idEstadistica
        obtenerFechaActual
        (resumenCategorias eventos)
        (eventoMaxCSV eventos)
        (eventoMinCSV eventos)
        (show (diaConMasActividad eventos))


--------------------------------------------------------------------------------
-- Nombre: generarId
--
-- Objetivo: genera un identificador único para la estadística
--
-- Entradas: lista de eventos
--
-- Salida: número entero como ID
--
-- Restricciones: ninguna
--------------------------------------------------------------------------------
generarId :: [Evento] -> Int
generarId eventos =
    let cantidadEventos = length eventos

        sumaTotales =  sum (map totalReal eventos)

        parteEntera = round sumaTotales

        mezcla = cantidadEventos * 37 + parteEntera

    in mezcla `mod` 9001


--------------------------------------------------------------------------------
-- Nombre: obtenerFechaActual
--
-- Objetivo: retorna la fecha del sistema usada para la estadística
--
-- Entradas: ninguna
--
-- Salida: fecha en formato entero
--------------------------------------------------------------------------------
obtenerFechaActual :: Int
obtenerFechaActual = 20260416 --fecha de asignacion del proyecto 


--------------------------------------------------------------------------------
-- Nombre: contarEventosPorCategoria
--
-- Objetivo: cuenta cuántos eventos hay por cada categoría
--
-- Entradas: lista de eventos
--
-- Salida: lista de pares (categoría, cantidad)
--
-- Restricciones:
--   - los eventos deben tener categoría válida
--------------------------------------------------------------------------------
contarEventosPorCategoria :: [Evento] -> [(String, Int)]
contarEventosPorCategoria eventos =
    let ordenar = sortBy (compare `on` categoria) eventos

        agrupar = groupBy ((==) `on` categoria) ordenar

        contar grupo = (show (categoria (head grupo)), length grupo)

    in map contar agrupar
    
--------------------------------------------------------------------------------
-- Nombre: buscarCategoria
--
-- Objetivo: obtiene la cantidad de eventos de una categoría específica
--
-- Entradas:
--   - nombre de categoría
--   - lista de conteos por categoría
--
-- Salida: cantidad de eventos de esa categoría
--------------------------------------------------------------------------------
buscarCategoria :: String -> [(String, Int)] -> Int
buscarCategoria categoriaBuscada lista =
    case lookup categoriaBuscada lista of
        Just valor -> valor
        Nothing    -> 0

--------------------------------------------------------------------------------
-- Nombre: resumenCategorias
--
-- Objetivo: genera un resumen textual de todas las categorías
--
-- Entradas: lista de eventos
--
-- Salida: texto con conteo por categoría
--------------------------------------------------------------------------------
resumenCategorias :: [Evento] -> String
resumenCategorias eventos =
    let
        resumen = contarEventosPorCategoria eventos
    in
        unlines
        [ "Apartado:      " ++ show (buscarCategoria "Apartado" resumen)
        , "Compra:        " ++ show (buscarCategoria "Compra" resumen)
        , "Devolución:    " ++ show (buscarCategoria "Devolucion" resumen)
        , "Seguimiento:   " ++ show (buscarCategoria "Seguimiento" resumen)
        , "Visualización: " ++ show (buscarCategoria "Visualizacion" resumen)
        ]


--------------------------------------------------------------------------------
-- Nombre: guardarCSV
--
-- Objetivo: guarda la estadística generada en un archivo CSV
--
-- Entradas:
--   - lista de eventos
--   - estadística generada
--
-- Salida: IO () que escribe en archivo
--------------------------------------------------------------------------------
guardarCSV :: [Evento] -> Estadistica -> IO ()
guardarCSV eventos estadistica = do

    let resumen = contarEventosPorCategoria eventos

        lineaCSV =
            show (estId estadistica) ++ "," ++
            show (fechaConsulta estadistica) ++ "," ++
            show (buscarCategoria "Apartado" resumen) ++ "," ++
            show (buscarCategoria "Compra" resumen) ++ "," ++
            show (buscarCategoria "Devolucion" resumen) ++ "," ++
            show (buscarCategoria "Seguimiento" resumen) ++ "," ++
            show (buscarCategoria "Visualizacion" resumen) ++ "," ++
            eventoMaxCSV eventos ++ "," ++
            eMinimoSinDev eventos ++ "," ++
            show (diaConMasActividad eventos)

    appendFile "data/estadisticas.csv" (lineaCSV ++ "\n")


--------------------------------------------------------------------------------
-- Nombre: diaConMasActividad
--
-- Objetivo: determina el día con mayor número de eventos registrados
--
-- Entradas: lista de eventos
--
-- Salida: timestamp del día más activo
--
-- Restricciones:
--   - la lista no debe estar vacía para un resultado válido
--------------------------------------------------------------------------------
diaConMasActividad :: [Evento] -> Int
diaConMasActividad [] = 0
diaConMasActividad eventos =
    let timestamps = sort (map timestamp eventos)

        grupos = group timestamps

        contarGrupo grupo =  (head grupo, length grupo)

        conteoPorDia = map contarGrupo grupos

        (diaMasActivo, _) = maximumBy (compare `on` snd) conteoPorDia

    in diaMasActivo