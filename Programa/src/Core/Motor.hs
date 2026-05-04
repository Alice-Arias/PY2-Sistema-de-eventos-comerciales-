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

--------------------------------------------------------------------------------
-- Nombre: cicloMenu
-- Entrada:
--   rutaArchivo: ubicación del archivo donde se guardan los eventos
--   eventos: lista de registros del sistema con todas las acciones realizadas
-- Salida:
--   ejecuta el menú principal del sistema en forma de ciclo continuo
-- Restricciones:
--   el sistema depende de entrada válida del usuario
--   el archivo debe existir o ser accesible
--------------------------------------------------------------------------------
cicloMenu :: FilePath -> [Evento] -> IO ()
cicloMenu rutaArchivo eventos = do

    mostrarMenu

    opcionUsuario <- pedirOpcionUsuario

    manejarOpcion rutaArchivo opcionUsuario eventos

--------------------------------------------------------------------------------
-- Nombre: manejarOpcion
-- Entrada:
--   rutaArchivo: ubicación del archivo de eventos
--   opcionUsuario: opción seleccionada por el usuario
--   eventos: lista de eventos del sistema
-- Salida:
--   ejecuta la acción correspondiente a la opción elegida
-- Restricciones:
--   si la opción no es válida, se muestra un error y vuelve al menú
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
-- Entrada:
--   rutaArchivo: ubicación del archivo de eventos
--   eventos: lista de registros del sistema
-- Salida:
--   actualiza los eventos, los transforma y los guarda en el archivo
-- Restricciones:
--   requiere acceso al archivo de datos
--------------------------------------------------------------------------------
opcionTransformar :: FilePath -> [Evento] -> IO ()
opcionTransformar rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos
    eventosTransformados <- menuTransformacion eventosActualizados
    guardarEventos rutaArchivo eventosTransformados
    cicloMenu rutaArchivo eventosTransformados

--------------------------------------------------------------------------------
-- Nombre: opcionAnalisis
-- Entrada:
--   rutaArchivo: ubicación del archivo de eventos
--   eventos: lista de registros del sistema
-- Salida:
--   muestra análisis general del sistema
-- Restricciones:
--   los eventos deben estar actualizados
--------------------------------------------------------------------------------
opcionAnalisis :: FilePath -> [Evento] -> IO ()
opcionAnalisis rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos
    menuAnalisis rutaArchivo eventosActualizados
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: opcionAnalisisTemporal
-- Entrada:
--   rutaArchivo: ubicación del archivo de eventos
--   eventos: lista de registros del sistema
-- Salida:
--   muestra análisis basado en el tiempo
-- Restricciones:
--   los eventos deben estar actualizados
--------------------------------------------------------------------------------
opcionAnalisisTemporal :: FilePath -> [Evento] -> IO ()
opcionAnalisisTemporal rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos
    menuAnalisisTemporal rutaArchivo eventosActualizados
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: opcionBusquedaRango
-- Entrada:
--   rutaArchivo: ubicación del archivo de eventos
--   eventos: lista de registros del sistema
-- Salida:
--   muestra eventos filtrados por rango de fechas ingresado por el usuario
-- Restricciones:
--   el usuario debe ingresar fechas válidas
--------------------------------------------------------------------------------
opcionBusquedaRango :: FilePath -> [Evento] -> IO ()
opcionBusquedaRango rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos

    putStrLn (titulo "========================================")
    putStrLn (titulo "   BÚSQUEDA POR RANGO DE FECHAS")
    putStrLn (titulo "========================================")

    fechaInicio <- pedirFechaValida "Ingrese fecha de inicio: "
    fechaFin <- pedirFechaValida "Ingrese fecha final: "

    putStrLn (separador "========================================")

    buscarPorRangoFechas eventosActualizados fechaInicio fechaFin
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: opcionEstadisticas
-- Entrada:
--   rutaArchivo: ubicación del archivo de eventos
--   eventos: lista de registros del sistema
-- Salida:
--   genera y muestra estadísticas del sistema
-- Restricciones:
--   los eventos deben estar actualizados
--------------------------------------------------------------------------------
opcionEstadisticas :: FilePath -> [Evento] -> IO ()
opcionEstadisticas rutaArchivo eventos = do
    eventosActualizados <- actualizarSistema rutaArchivo eventos

    putStrLn (titulo "========================================")
    putStrLn (titulo "   GENERANDO ESTADÍSTICAS")
    putStrLn (titulo "========================================")

    _ <- generarEstadisticas eventosActualizados
    cicloMenu rutaArchivo eventosActualizados

--------------------------------------------------------------------------------
-- Nombre: pedirFechaValida
-- Entrada:
--   mensaje: texto que se muestra al usuario
-- Salida:
--   fecha ingresada en formato correcto
-- Restricciones:
--   el usuario debe ingresar una fecha válida
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
-- Entrada:
--   rutaArchivo: ubicación del archivo de eventos
--   eventos: lista de registros del sistema
-- Salida:
--   lista actualizada de eventos incluyendo los nuevos generados
-- Restricciones:
--   requiere acceso de lectura y escritura del archivo
--------------------------------------------------------------------------------
actualizarSistema :: FilePath -> [Evento] -> IO [Evento]
actualizarSistema rutaArchivo eventos = do
    nuevosEventos <- generarEventos eventos
    mapM_ (agregarEventoSeguro rutaArchivo) nuevosEventos
    mostrarResumenActualizacion (length nuevosEventos)
    leerEventosSeguro rutaArchivo