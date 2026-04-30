module Core.Motor where

import UI.Menu
import Services.GeneradorEventos
import Types.Evento
import Utils.CSV
import UI.MenuTransformacion (menuTransformacion)
import UI.MenuAnalisisTemporal

-- =========================
-- CICLO PRINCIPAL
-- =========================

-- Nombre: cicloMenu
-- Descripción: Controla el flujo principal del sistema, mostrando el menú e interpretando la opción seleccionada por el usuario.
-- Entradas: FilePath ruta del archivo CSV, lista de eventos actuales.
-- Salidas: IO () con ejecución continua del sistema.
-- Validaciones: La ruta debe existir y la lista de eventos debe ser válida.
cicloMenu :: FilePath -> [Evento] -> IO ()
cicloMenu ruta eventos = do
    mostrarMenu
    opcion <- getLine
    manejar ruta opcion eventos

-- Nombre: manejar
-- Descripción: Ejecuta la funcionalidad correspondiente según la opción seleccionada del menú principal.
-- Entradas: FilePath ruta del CSV, String opción del usuario, lista de eventos.
-- Salidas: IO () con ejecución de la acción seleccionada.
-- Validaciones: La opción debe estar entre "1" y "6"; en caso contrario se muestra error.
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
            putStrLn "=================================="
            putStrLn "    ANÁLISIS DE DATOS           "
            putStrLn "=================================="
            putStrLn ""
            cicloMenu ruta eventosActualizados


        "3" -> do
            eventosActualizados <- actualizarSistema ruta eventos

            menuAnalisisTemporal ruta eventosActualizados

            cicloMenu ruta eventosActualizados


        "4" -> do
            eventosActualizados <- actualizarSistema ruta eventos
            putStrLn "=================================="
            putStrLn "    BÚSQUEDA DE EVENTOS           "
            putStrLn "=================================="
            putStrLn ""
            cicloMenu ruta eventosActualizados


        "5" -> do
            eventosActualizados <- actualizarSistema ruta eventos
            putStrLn "=================================="
            putStrLn "    GENERACIÓN DE ESTADÍSTICAS     "
            putStrLn "=================================="
            putStrLn ""
            cicloMenu ruta eventosActualizados


        "6" -> do 
            putStrLn "=================================="
            putStrLn "    SALIENDO DEL SISTEMA        "
            putStrLn "=================================="
            putStrLn ""
            return ()

        _ -> do
            putStrLn "=================================="
            putStrLn "    OPCIÓN INVÁLIDA              "
            putStrLn "=================================="
            putStrLn ""
            cicloMenu ruta eventos

-- Nombre: actualizarSistema
-- Descripción: Genera nuevos eventos, los guarda en el CSV y retorna la lista actualizada desde el archivo.
-- Entradas: FilePath ruta del CSV, lista de eventos actuales.
-- Salidas: Lista de eventos actualizada desde el CSV.
-- Validaciones: Los eventos generados se deben poder persistir correctamente en el archivo.
actualizarSistema :: FilePath -> [Evento] -> IO [Evento]
actualizarSistema ruta eventos = do
    nuevos <- generarEventos eventos

    mapM_ (agregarEventoSeguro ruta) nuevos --mapM_ es para aplicar la función a cada elemento de la lista sin preocuparse por el resultado

    let cantidad = length nuevos

    putStrLn "===================================="
    putStrLn "        ACTUALIZACIÓN SISTEMA       "
    putStrLn "===================================="
    putStrLn (" Eventos agregados: " ++ show cantidad)
    putStrLn "===================================="
    putStrLn ""

    leerEventosSeguro ruta