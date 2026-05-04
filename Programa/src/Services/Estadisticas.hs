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
-- Entrada: lista de eventos del sistema
-- Salida:
--   Genera una estadística completa, la guarda en archivo y la muestra en pantalla
-- Restricciones:
--   - Los eventos deben tener datos válidos
--   - Depende de otros módulos de cálculo y formato
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
-- Entrada: lista de eventos del sistema
-- Salida: estructura final de estadística del sistema
-- Restricciones:
--   - Requiere que los eventos tengan información válida
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
-- Entrada: lista de eventos
-- Salida: número identificador de la estadística
-- Restricciones: ninguna
--------------------------------------------------------------------------------
generarId :: [Evento] -> Int
generarId eventos = (length eventos * 37 + round (sum (map totalReal eventos))) `mod` 9001


--------------------------------------------------------------------------------
-- Nombre: obtenerFechaActual
-- Entrada: ninguna
-- Salida: fecha fija del sistema
--------------------------------------------------------------------------------
obtenerFechaActual :: Int
obtenerFechaActual = 20260416


--------------------------------------------------------------------------------
-- Nombre: contarEventosPorCategoria
-- Entrada: lista de eventos
-- Salida: lista con cantidad de eventos por categoría
-- Restricciones: los eventos deben tener categoría válida
--------------------------------------------------------------------------------
contarEventosPorCategoria :: [Evento] -> [(String, Int)]
contarEventosPorCategoria =
    map (\grupo -> (show (categoria (head grupo)), length grupo))
    . groupBy ((==) `on` (show . categoria))
    . sortBy (compare `on` (show . categoria))


--------------------------------------------------------------------------------
-- Nombre: buscarCategoria
-- Entrada:
--   nombre de categoría
--   lista de conteos por categoría
-- Salida: cantidad de eventos de esa categoría
--------------------------------------------------------------------------------
buscarCategoria :: String -> [(String, Int)] -> Int
buscarCategoria categoriaBuscada lista = maybe 0 id (lookup categoriaBuscada lista)


--------------------------------------------------------------------------------
-- Nombre: resumenCategorias
-- Entrada: lista de eventos
-- Salida: texto con resumen de todas las categorías
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
-- Entrada: lista de eventos y estadística generada
-- Salida: guarda la estadística en un archivo CSV
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
-- Entrada: lista de eventos
-- Salida: día (timestamp) con más actividad registrada
-- Restricciones: lista no vacía para resultado válido
--------------------------------------------------------------------------------
diaConMasActividad :: [Evento] -> Int
diaConMasActividad [] = 0
diaConMasActividad eventos =
    let 
        fechasOrdenadas = sort (map timestamp eventos)
        gruposFechas = group fechasOrdenadas
        conteoDias = map (\g -> (head g, length g)) gruposFechas
        (dia, _) = maximumBy (compare `on` snd) conteoDias
    in 
        dia