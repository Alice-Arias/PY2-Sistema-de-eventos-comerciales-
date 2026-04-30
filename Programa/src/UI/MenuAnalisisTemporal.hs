module UI.MenuAnalisisTemporal where

import Types.Evento
import Core.AnalisisTemporal
import Services.GeneradorEventos (generarEventos)


menuAnalisisTemporal :: FilePath -> [Evento] -> IO ()
menuAnalisisTemporal ruta eventos = do

    putStrLn "\n========================================"
    putStrLn "        ANÁLISIS TEMPORAL"
    putStrLn "========================================"
    putStrLn "1. Mes y día más activo"
    putStrLn "2. Eventos extremos"
    putStrLn "3. Resumen mensual"
    putStrLn "4. Salir"
    putStrLn "========================================"
    putStrLn "Seleccione una opción:"

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
        putStrLn "\n========================================"
        putStrLn "   SALIENDO DEL ANÁLISIS TEMPORAL"
        putStrLn "========================================"

    _ -> do
        putStrLn "\nOpción inválida"
        menuAnalisisTemporal ruta eventos