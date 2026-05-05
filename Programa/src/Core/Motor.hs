module Core.Motor where

import UI.Menus

import Services.GeneradorEventos
import Services.BusquedaRangFechas
import Services.Estadisticas

import Types.Modelos
import Types.Fecha

import UI.Interfaz
import Utils.CSV
import Utils.Colores
import Utils.Colores (subtitulo)

--------------------------------------------------------------------------------
-- Nombre: cicloMenu
--
-- Objetivo: controla el flujo principal del sistema mediante un menú interactivo
--
-- Entradas:
--   - rutaArchivo: ubicación del archivo de eventos
--   - eventos: estado actual del sistema
--
-- Salida: IO () con ejecución continua del sistema
--
-- Restricciones:
--   - requiere interacción válida del usuario
--   - el archivo de eventos debe existir o ser accesible
--------------------------------------------------------------------------------
cicloMenu :: FilePath -> [Evento] -> IO ()
cicloMenu rutaArchivo eventos = do

    mostrarMenu

    opcionUsuario <- pedirOpcionUsuario

    manejarOpcion rutaArchivo opcionUsuario eventos

--------------------------------------------------------------------------------
-- Nombre: manejarOpcion
--
-- Objetivo: ejecuta la acción correspondiente según la opción del usuario
--
-- Entradas:
--   - rutaArchivo: ubicación del archivo de eventos
--   - opcionUsuario: opción seleccionada
--   - eventos: estado actual del sistema
--
-- Salida: IO () con la operación seleccionada o retorno al menú
--
-- Restricciones:
--   - si la opción es inválida, se muestra error y se reinicia el menú
--------------------------------------------------------------------------------
manejarOpcion :: FilePath -> String -> [Evento] -> IO ()
manejarOpcion rutaArchivo opcionUsuario eventos =
    case opcionUsuario of

        "1" -> opcionTransformar rutaArchivo eventos
        "2" -> opcionAnalisis rutaArchivo eventos
        "3" -> opcionAnalisisTemporal rutaArchivo eventos
        "4" -> opcionBusquedaRango rutaArchivo eventos
        "5" -> opcionEstadisticas rutaArchivo eventos
        "6" -> mostrarSalidaSistema >> return ()

        _   -> do
            mostrarError
            cicloMenu rutaArchivo eventos

--------------------------------------------------------------------------------
-- Nombre: opcionTransformar
--
-- Objetivo: genera nuevos eventos, los procesa y los guarda en el sistema
--
-- Entradas: ruta del archivo y estado actual del sistema
--
-- Salida: IO () con sistema actualizado
--
-- Restricciones: requiere acceso al archivo de persistencia
--------------------------------------------------------------------------------
opcionTransformar :: FilePath -> [Evento] -> IO ()
opcionTransformar rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos
    eventosTransformados <- menuTransformacion eventosActualizados
    guardarEventos rutaArchivo eventosTransformados
    cicloMenu rutaArchivo eventosTransformados

--------------------------------------------------------------------------------
-- Nombre: opcionAnalisis
--
-- Objetivo: ejecuta análisis general sobre los eventos actuales
--
-- Entradas: ruta del archivo y eventos actuales
--
-- Salida: IO () con resultados en pantalla
--
-- Restricciones: los eventos deben estar actualizados
--------------------------------------------------------------------------------
opcionAnalisis :: FilePath -> [Evento] -> IO ()
opcionAnalisis rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos
    menuAnalisis rutaArchivo eventosActualizados
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: opcionAnalisisTemporal
--
-- Objetivo: ejecuta análisis basado en comportamiento temporal de los eventos
--
-- Entradas: ruta del archivo y eventos actuales
--
-- Salida: IO () con análisis temporal
--
-- Restricciones: requiere eventos actualizados
--------------------------------------------------------------------------------
opcionAnalisisTemporal :: FilePath -> [Evento] -> IO ()
opcionAnalisisTemporal rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos
    menuAnalisisTemporal rutaArchivo eventosActualizados
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: opcionBusquedaRango
--
-- Objetivo: filtra y muestra eventos dentro de un rango de fechas ingresado
--
-- Entradas: ruta del archivo y eventos actuales
--
-- Salida: IO () con resultados filtrados
--
-- Restricciones:
--   - el usuario debe ingresar fechas válidas
--------------------------------------------------------------------------------
opcionBusquedaRango :: FilePath -> [Evento] -> IO ()
opcionBusquedaRango rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos

    putStrLn (titulo "\n════════════════════════════════════════")
    putStrLn (subtitulo "   BÚSQUEDA POR RANGO DE FECHAS")
    putStrLn (titulo "════════════════════════════════════════")

    fechaInicio <- pedirFechaValida "Ingrese fecha de inicio: "
    fechaFin <- pedirFechaValida "Ingrese fecha final: "

    putStrLn (separador "════════════════════════════════════════")

    buscarPorRangoFechas eventosActualizados fechaInicio fechaFin
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: opcionEstadisticas
--
-- Objetivo: genera estadísticas generales del sistema
--
-- Entradas: ruta del archivo y eventos actuales
--
-- Salida: IO () con reporte estadístico
--
-- Restricciones: requiere eventos actualizados
--------------------------------------------------------------------------------
opcionEstadisticas :: FilePath -> [Evento] -> IO ()
opcionEstadisticas rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos

    putStrLn (titulo "════════════════════════════════════════")
    putStrLn (subtitulo "   GENERANDO ESTADÍSTICAS")
    putStrLn (titulo "════════════════════════════════════════")

    _ <- generarEstadisticas eventosActualizados
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: pedirFechaValida
--
-- Objetivo: solicita al usuario una fecha válida en formato dd-mm-yyyy
--
-- Entradas: mensaje a mostrar
--
-- Salida: fecha válida como String
--
-- Restricciones: reintenta hasta que el formato sea correcto
--------------------------------------------------------------------------------
pedirFechaValida :: String -> IO String
pedirFechaValida mensaje = do

    putStrLn (inputMsg mensaje)
    entradaUsuario <- getLine

    case parseFecha entradaUsuario of

        Just _  -> return entradaUsuario

        Nothing -> do
            putStrLn (errorMsg "Formato inválido. Usa dd-mm-yyyy")
            pedirFechaValida mensaje

--------------------------------------------------------------------------------
-- Nombre: actualizarSistema
--
-- Objetivo: genera nuevos eventos y actualiza el archivo del sistema
--
-- Entradas: ruta del archivo y eventos actuales
--
-- Salida: lista actualizada de eventos
--
-- Restricciones:
--   - requiere lectura y escritura del archivo de eventos
--------------------------------------------------------------------------------
actualizarSistema :: FilePath -> [Evento] -> IO [Evento]
actualizarSistema rutaArchivo eventos = do
    nuevosEventos <- generarEventos eventos
    mapM_ (agregarEventoSeguro rutaArchivo) nuevosEventos
    mostrarResumenActualizacion (length nuevosEventos)
    leerEventosSeguro rutaArchivo