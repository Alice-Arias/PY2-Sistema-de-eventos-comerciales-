module UI.MenuTransformacion where

import Types.Evento
import Core.Impuestos
import Core.Etiquetas
import UI.Reportes
import Utils.Colores

menuTransformacion :: [Evento] -> IO [Evento]
menuTransformacion eventos = do

    putStrLn (separador "==================================")
    putStrLn (titulo "   TRANSFORMACIÓN DE EVENTOS")
    putStrLn (separador "==================================")

    putStrLn (opcion "1. Aplicar impuestos + etiquetas")
    putStrLn (opcion "2. Solo impuestos")
    putStrLn (opcion "3. Solo etiquetas")

    putStrLn (separador "==================================")
    putStrLn (inputMsg "Seleccione una opción:")

    opcion <- getLine

    case opcion of

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
            let eventosConEtiquetas = etiquetarAltoValor eventos

            putStrLn (okMsg "\nAplicando etiquetas...\n")
            reporteEtiquetas eventosConEtiquetas
            return eventosConEtiquetas

        _ -> do
            putStrLn (errorMsg "\nOpción inválida")
            menuTransformacion eventos