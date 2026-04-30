module UI.Menu where

import Utils.Colores

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