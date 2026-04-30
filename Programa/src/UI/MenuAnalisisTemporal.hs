module UI.MenuAnalisisTemporal where

import Types.Evento
import Core.AnalisisTemporal
import Services.GeneradorEventos (generarEventos)
import Utils.Colores
import System.IO (hFlush, stdout)

menuAnalisisTemporal :: FilePath -> [Evento] -> IO ()
menuAnalisisTemporal ruta eventos = do

    putStrLn (separador "\n========================================")
    putStrLn (titulo "        ANÁLISIS TEMPORAL")
    putStrLn (separador "========================================")

    putStrLn (opcion "1. Mes y día más activo")
    putStrLn (opcion "2. Eventos extremos")
    putStrLn (opcion "3. Resumen por intervalo de días")
    putStrLn (warningMsg "4. Salir")

    putStrLn (separador "========================================")

    putStr (inputMsg "Seleccione una opción: ")
    hFlush stdout
    opcion <- getLine

    procesarOpcion ruta opcion eventos

procesarOpcion :: FilePath -> String -> [Evento] -> IO ()
procesarOpcion ruta opcion eventos = case opcion of

    "1" -> do
        analisisMesDia eventos
        menuAnalisisTemporal ruta eventos

    "2" -> do
        analisisExtremos eventos
        menuAnalisisTemporal ruta eventos

    "3" -> do
        analisisResumen eventos
        menuAnalisisTemporal ruta eventos

    "4" -> do
        putStrLn ("\n" ++ titulo "========================================")
        putStrLn (warningMsg "   SALIENDO DEL ANÁLISIS TEMPORAL")
        putStrLn (titulo "========================================")

    _ -> do
        putStrLn ("\n" ++ errorMsg "Opción inválida")
        menuAnalisisTemporal ruta eventos