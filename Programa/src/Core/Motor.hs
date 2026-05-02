module Core.Motor where

import UI.Menu
import Services.GeneradorEventos
import Types.Evento
import Utils.CSV
import UI.MenuTransformacion (menuTransformacion)
import UI.MenuAnalisisTemporal (menuAnalisisTemporal)
import UI.MenuAnalisis (menuAnalisis)
import Services.BusquedaRangFechas (buscarPorRangoFechas)
import Utils.FechaSegura (parseFecha)
import Utils.Colores

cicloMenu :: FilePath -> [Evento] -> IO ()
cicloMenu rutaArchivo eventos = do
    mostrarMenu
    opcion <- pedirOpcionUsuario
    manejarOpcion rutaArchivo opcion eventos


manejarOpcion :: FilePath -> String -> [Evento] -> IO ()
manejarOpcion ruta opcion eventos =
    case opcion of

        "1" -> opcionTransformar ruta eventos

        "2" -> opcionAnalisis ruta eventos

        "3" -> opcionAnalisisTemporal ruta eventos

        "4" -> opcionBusquedaRango ruta eventos

        "5" -> opcionSimple ruta eventos "GENERACIÓN DE ESTADÍSTICAS"

        "6" -> mostrarSalidaSistema

        _   -> do
            mostrarError
            cicloMenu ruta eventos



opcionTransformar :: FilePath -> [Evento] -> IO ()
opcionTransformar ruta eventos = do
    eventosActualizados <- actualizarSistema ruta eventos
    eventosTransformados <- menuTransformacion eventosActualizados
    guardarEventos ruta eventosTransformados
    cicloMenu ruta eventosTransformados

opcionSimple :: FilePath -> [Evento] -> String -> IO ()
opcionSimple ruta eventos tituloSeccion = do
    eventosActualizados <- actualizarSistema ruta eventos
    mostrarTitulo tituloSeccion
    cicloMenu ruta eventosActualizados

opcionAnalisisTemporal :: FilePath -> [Evento] -> IO ()
opcionAnalisisTemporal ruta eventos = do
    eventosActualizados <- actualizarSistema ruta eventos
    menuAnalisisTemporal ruta eventosActualizados
    cicloMenu ruta eventosActualizados

opcionAnalisis :: FilePath -> [Evento] -> IO ()
opcionAnalisis ruta eventos = do
    eventosActualizados <- actualizarSistema ruta eventos
    menuAnalisis ruta eventosActualizados
    cicloMenu ruta eventosActualizados

opcionBusquedaRango :: FilePath -> [Evento] -> IO ()
opcionBusquedaRango ruta eventos = do
    eventosActualizados <- actualizarSistema ruta eventos
    putStrLn (titulo "========================================")
    putStrLn (titulo "   BÚSQUEDA POR RANGO DE FECHAS")
    putStrLn (titulo "========================================")
    inicio <- pedirFechaValida "Ingrese fecha inicio : "
    fin    <- pedirFechaValida "Ingrese fecha fin : "
    putStrLn (separador "========================================")
    buscarPorRangoFechas eventosActualizados inicio fin
    cicloMenu ruta eventosActualizados


pedirFechaValida :: String -> IO String
pedirFechaValida mensaje = do
    putStrLn (inputMsg mensaje)
    entrada <- getLine
    case parseFecha entrada of
        Just _  -> return entrada
        Nothing -> do
            putStrLn (errorMsg "Formato inválido. Usa dd-mm-yyyy")
            pedirFechaValida mensaje

-- =========================
-- LÓGICA SISTEMA
-- =========================

actualizarSistema :: FilePath -> [Evento] -> IO [Evento]
actualizarSistema ruta eventos = do
    nuevosEventos <- generarEventos eventos
    mapM_ (agregarEventoSeguro ruta) nuevosEventos
    mostrarResumenActualizacion (length nuevosEventos)
    leerEventosSeguro ruta