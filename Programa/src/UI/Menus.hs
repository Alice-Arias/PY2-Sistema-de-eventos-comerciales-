module UI.Menus
(
    menuTransformacion,
    menuAnalisis,
    menuAnalisisTemporal
) where

import Types.Modelos
import Core.Impuestos
import Core.Etiquetas
import Core.AnalisisTemporal
import Services.Analisis.MontoTotal
import Services.Analisis.PromedioCategoriaAnio
import UI.Interfaz
import Utils.Colores
import System.IO 
import Utils.Colores (subtitulo)

--------------------------------------------------------------------------------
-- MENU: TRANSFORMACIÓN DE EVENTOS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: menuTransformacion
-- Entrada: lista de eventos
-- Salida: lista de eventos transformados o sin cambios
-- Restricciones:
--   - No modifica datos si el usuario no elige opción válida
--------------------------------------------------------------------------------
menuTransformacion :: [Evento] -> IO [Evento]
menuTransformacion eventos = do

    putStrLn (separador "\n══════════════════════════════════════")
    putStrLn (titulo "    TRANSFORMACIÓN DE EVENTOS")
    putStrLn (separador "══════════════════════════════════════")

    putStrLn (opcion "1. Aplicar impuestos + etiquetas")
    putStrLn (opcion "2. Solo impuestos")
    putStrLn (opcion "3. Solo etiquetas")

    putStrLn (separador "══════════════════════════════════════")
    putStrLn (inputMsg "Seleccione una opción:")
    opcionUsuario <- getLine

    case opcionUsuario of

        "1" -> do
            let eventosConImpuestos = aplicarImpuestos eventos
            let eventosFinales = etiquetarAltoValor eventosConImpuestos

            putStrLn (okMsg "\nAplicando impuestos y etiquetas...\n")
            reporteCompleto eventosFinales

            return eventosFinales

        "2" -> do
            let eventosConImpuestos = aplicarImpuestos eventos

            putStrLn (okMsg "\nAplicando impuestos...\n")
            reporteImpuestos eventosConImpuestos

            return eventosConImpuestos

        "3" -> do
            let eventosEtiquetados = etiquetarAltoValor eventos

            putStrLn (okMsg "\nAplicando etiquetas...\n")
            reporteEtiquetas eventosEtiquetados

            return eventosEtiquetados

        _ -> do
            putStrLn (errorMsg "\nOpción inválida")
            menuTransformacion eventos


--------------------------------------------------------------------------------
-- MENU: ANÁLISIS DE DATOS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: menuAnalisis
-- Entrada: ruta del archivo (no usada) y lista de eventos
-- Salida: ejecuta análisis seleccionado por el usuario
-- Restricciones:
--   - Solo muestra resultados, no modifica datos
--------------------------------------------------------------------------------
menuAnalisis :: FilePath -> [Evento] -> IO ()
menuAnalisis _ eventos = do

    putStrLn (titulo "\n══════════════════════════════════════")
    putStrLn (subtitulo "      ANÁLISIS DE DATOS")
    putStrLn (titulo "══════════════════════════════════════")

    putStrLn (opcion "1. Monto total")
    putStrLn (opcion "2. Promedio por categoría por año")

    putStrLn (titulo "══════════════════════════════════════")
    putStrLn (inputMsg "Seleccione una opción: ")

    opcionUsuario <- getLine

    case opcionUsuario of
        "1" -> montoTotal eventos
        "2" -> promedioCategoriaPorAnio eventos
        _   -> putStrLn (errorMsg "Opción inválida")


--------------------------------------------------------------------------------
-- MENU: ANÁLISIS TEMPORAL
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: menuAnalisisTemporal
-- Entrada: ruta del archivo y lista de eventos
-- Salida: ejecuta análisis temporal seleccionado
-- Restricciones:
--   - Permite ejecución repetida hasta salir
--------------------------------------------------------------------------------
menuAnalisisTemporal :: FilePath -> [Evento] -> IO ()
menuAnalisisTemporal ruta eventos = do

    putStrLn (separador "\n══════════════════════════════════════")
    putStrLn (titulo "        ANÁLISIS TEMPORAL")
    putStrLn (separador "══════════════════════════════════════")

    putStrLn (opcion "1. Mes y día más activo")
    putStrLn (opcion "2. Eventos extremos")
    putStrLn (opcion "3. Resumen por intervalo de días")
    putStrLn (warningMsg "4. Salir")

    putStrLn (separador "══════════════════════════════════════")
    putStr (inputMsg "Seleccione una opción: ")
    hFlush stdout

    opcionUsuario <- getLine

    case opcionUsuario of
        "1" -> analisisMesDia eventos >> menuAnalisisTemporal ruta eventos
        "2" -> analisisExtremos eventos >> menuAnalisisTemporal ruta eventos
        "3" -> analisisResumen eventos >> menuAnalisisTemporal ruta eventos
        "4" -> putStrLn (okMsg "Saliendo...")
        _   -> putStrLn (errorMsg "Opción inválida") >> menuAnalisisTemporal ruta eventos