module Core.Motor where

import UI.Menu
import Services.GeneradorEventos
import Types.Evento
import Utils.CSV
import UI.MenuTransformacion (menuTransformacion)
import UI.MenuAnalisisTemporal
import Utils.Colores
import System.IO (hFlush, stdout)
-- =========================
-- CICLO PRINCIPAL
-- =========================
cicloMenu :: FilePath -> [Evento] -> IO ()
cicloMenu ruta eventos = do
    mostrarMenu
    putStrLn ""
    putStr (inputMsg "Seleccione una opción: ")
    hFlush stdout
    opcion <- getLine
    putStrLn ""
    manejar ruta opcion eventos

-- =========================
-- MANEJO DE OPCIONES
-- =========================

manejar :: FilePath -> String -> [Evento] -> IO ()
manejar ruta op eventos =
    case op of

        "1" -> do
            eventosActualizados <- actualizarSistema ruta eventos

            eventosTransformados <- menuTransformacion eventosActualizados

            guardarEventos ruta eventosTransformados

            cicloMenu ruta eventosTransformados

        "2" -> do
            eventosActualizados <- actualizarSistema ruta eventos

            putStrLn (titulo "==================================")
            putStrLn (subtitulo "       ANÁLISIS DE DATOS")
            putStrLn (titulo "==================================")
            putStrLn ""

            cicloMenu ruta eventosActualizados

        "3" -> do
            eventosActualizados <- actualizarSistema ruta eventos

            menuAnalisisTemporal ruta eventosActualizados

            cicloMenu ruta eventosActualizados

        "4" -> do
            eventosActualizados <- actualizarSistema ruta eventos

            putStrLn (titulo "==================================")
            putStrLn (subtitulo "     BÚSQUEDA DE EVENTOS")
            putStrLn (titulo "==================================")
            putStrLn ""

            cicloMenu ruta eventosActualizados

        "5" -> do
            eventosActualizados <- actualizarSistema ruta eventos

            putStrLn (titulo "==================================")
            putStrLn (subtitulo "   GENERACIÓN DE ESTADÍSTICAS")
            putStrLn (titulo "==================================")
            putStrLn ""

            cicloMenu ruta eventosActualizados

        "6" -> do
            putStrLn (titulo "==================================")
            putStrLn (okMsg "     SALIENDO DEL SISTEMA")
            putStrLn (titulo "==================================")
            putStrLn ""
            return ()

        _ -> do
            putStrLn (titulo "==================================")
            putStrLn (errorMsg "     OPCIÓN INVÁLIDA")
            putStrLn (titulo "==================================")
            putStrLn ""

            cicloMenu ruta eventos

-- =========================
-- ACTUALIZACIÓN SISTEMA
-- =========================

actualizarSistema :: FilePath -> [Evento] -> IO [Evento]
actualizarSistema ruta eventos = do
    nuevos <- generarEventos eventos

    mapM_ (agregarEventoSeguro ruta) nuevos

    let cantidad = length nuevos

    putStrLn (titulo "====================================")
    putStrLn (subtitulo "        ACTUALIZACIÓN SISTEMA")
    putStrLn (titulo "====================================")

    putStrLn (okMsg ("Eventos agregados: " ++ show cantidad))

    putStrLn (titulo "====================================")
    putStrLn ""

    leerEventosSeguro ruta