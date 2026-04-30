module UI.MenuTransformacion where

import Types.Evento
import Core.Impuestos
import Core.Etiquetas
import UI.Reportes

menuTransformacion :: [Evento] -> IO [Evento]
menuTransformacion eventos = do

    putStrLn "=================================="
    putStrLn "   TRANSFORMACIÓN DE EVENTOS"
    putStrLn "=================================="
    putStrLn "1. Aplicar impuestos + etiquetas"
    putStrLn "2. Solo impuestos"
    putStrLn "3. Solo etiquetas"
    putStrLn "=================================="

    opcion <- getLine

    case opcion of

        "1" -> do
            let eventosConImpuestos = aplicarImpuestos eventos
            let eventosFinales = etiquetarAltoValor eventosConImpuestos

            reporteCompleto eventosFinales
            return eventosFinales

        "2" -> do
            let eventosConImpuestos = aplicarImpuestos eventos

            reporteImpuestos eventosConImpuestos
            return eventosConImpuestos

        "3" -> do
            let eventosConEtiquetas = etiquetarAltoValor eventos

            reporteEtiquetas eventosConEtiquetas
            return eventosConEtiquetas

        _ -> do
            putStrLn "Opción inválida"
            menuTransformacion eventos