module UI.MenuTransformacion where

import Types.Evento
import Core.Transformacion

-- Nombre: menuTransformacion
-- Descripción: Muestra el menú de transformación y permite aplicar impuestos, etiquetas o ambos a la lista de eventos.
-- Entradas: Lista de eventos.
-- Salidas: IO [Evento] con la lista de eventos actualizada según la opción seleccionada.
-- Validaciones: La opción ingresada debe ser válida (1, 2, 3 o salir implícito por recursión en caso de error).
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