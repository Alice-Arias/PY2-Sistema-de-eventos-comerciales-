module Core.Motor where

import UI.Menu
import Services.GeneradorEventos
import Types.Evento
import Utils.CSV

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
            putStrLn "Eventos transformados"
            cicloMenu ruta eventosActualizados

        "2" -> do
            eventosActualizados <- actualizarSistema ruta eventos
            putStrLn "Analizando datos..."
            cicloMenu ruta eventosActualizados


        "3" -> do
            eventosActualizados <- actualizarSistema ruta eventos
            putStrLn "Analizando comportamiento temporal..."
            cicloMenu ruta eventosActualizados


        "4" -> do
            eventosActualizados <- actualizarSistema ruta eventos
            putStrLn "Ejecutando búsqueda..."
            cicloMenu ruta eventosActualizados


        "5" -> do
            eventosActualizados <- actualizarSistema ruta eventos
            putStrLn "Generando estadísticas..."
            cicloMenu ruta eventosActualizados


        "6" -> putStrLn "Saliendo del sistema..."

        _ -> do
            putStrLn "Opción inválida"
            cicloMenu ruta eventos

-- Nombre: actualizarSistema
-- Descripción: Genera nuevos eventos, los guarda en el CSV y retorna la lista actualizada desde el archivo.
-- Entradas: FilePath ruta del CSV, lista de eventos actuales.
-- Salidas: Lista de eventos actualizada desde el CSV.
-- Validaciones: Los eventos generados se deben poder persistir correctamente en el archivo.
actualizarSistema :: FilePath -> [Evento] -> IO [Evento]
actualizarSistema ruta eventos = do
    nuevos <- generarEventos eventos
    mapM_ (agregarEventoSeguro ruta) nuevos

    putStrLn ("Se agregaron " ++ show (length nuevos) ++ " nuevos eventos al sistema.")

    leerEventosSeguro ruta