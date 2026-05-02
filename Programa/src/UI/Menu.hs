module UI.Menu where

import Utils.Colores
import System.IO (hFlush, stdout)


mostrarBienvenida :: IO ()
mostrarBienvenida = do
    putStrLn ""
    putStrLn (separador "========================================")
    putStrLn (titulo "        SISTEMA DE EVENTOS")
    putStrLn (separador "========================================")
    putStrLn (texto "        Bienvenido al sistema")
    putStrLn ""


mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn (separador "========================================")
    putStrLn (titulo "            MENÚ PRINCIPAL")
    putStrLn (separador "========================================")

    putStrLn (opcion " 1. Transformación de eventos")
    putStrLn (opcion " 2. Análisis de datos")
    putStrLn (opcion " 3. Análisis temporal")
    putStrLn (opcion " 4. Búsqueda")
    putStrLn (opcion " 5. Estadísticas")
    putStrLn (opcion " 6. Salir")

    putStrLn (separador "========================================")


pedirOpcionUsuario :: IO String
pedirOpcionUsuario = do
    putStrLn ""
    putStr (inputMsg "Seleccione una opción: ")
    hFlush stdout
    opcion <- getLine
    putStrLn ""
    return opcion


mostrarTitulo :: String -> IO ()
mostrarTitulo textoTitulo = do
    putStrLn (titulo "==================================")
    putStrLn (subtitulo ("       " ++ textoTitulo))
    putStrLn (titulo "==================================")
    putStrLn ""

mostrarResumenActualizacion :: Int -> IO ()
mostrarResumenActualizacion cantidad = do
    putStrLn (titulo "====================================")
    putStrLn (subtitulo "        ACTUALIZACIÓN SISTEMA")
    putStrLn (titulo "====================================")
    putStrLn (okMsg ("Eventos agregados: " ++ show cantidad))
    putStrLn (titulo "====================================")
    putStrLn ""

mostrarError :: IO ()
mostrarError = do
    putStrLn (titulo "==================================")
    putStrLn (errorMsg "     OPCIÓN INVÁLIDA")
    putStrLn (titulo "==================================")
    putStrLn ""

mostrarSalidaSistema :: IO ()
mostrarSalidaSistema = do
    putStrLn (titulo "==================================")
    putStrLn (okMsg "     SALIENDO DEL SISTEMA")
    putStrLn (titulo "==================================")
    putStrLn ""