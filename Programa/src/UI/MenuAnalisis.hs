module UI.MenuAnalisis where

import Types.Evento
import Services.Analisis.MontoTotal
import Services.Analisis.PromedioCategoriaAnio
import Utils.Colores

menuAnalisis :: FilePath -> [Evento] -> IO ()
menuAnalisis _ eventos = do

    putStrLn (titulo "\n================================")
    putStrLn (titulo "      ANÁLISIS DE DATOS")
    putStrLn (titulo "================================")

    putStrLn (opcion "1. Monto total")
    putStrLn (opcion "2. Promedio por categoría por año")

    putStrLn (titulo "================================")
    putStrLn (inputMsg "Seleccione una opción: ")

    opcion <- getLine

    case opcion of
        "1" -> montoTotal eventos
        "2" -> promedioCategoriaPorAnio eventos
        _   -> putStrLn (errorMsg "Opción inválida")