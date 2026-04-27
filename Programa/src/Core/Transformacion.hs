module Core.Transformacion where

import Types.Evento
import Data.List (nub)
import Types.Categoria

-- Nombre: aplicarImpuestos
-- Descripción: Aplica un impuesto del 13% únicamente a eventos de tipo COMPRA.
-- Entradas: Lista de eventos.
-- Salidas: Lista de eventos con impuesto actualizado.
-- Validaciones: Solo se aplica impuesto si la categoría es COMPRA; los demás eventos quedan con impuesto = 0.
aplicarImpuestos :: [Evento] -> [Evento]
aplicarImpuestos = map aplicar
    where
    aplicar evento
        | esCompra evento = evento { impuesto = calcularImpuesto evento }
        | otherwise = evento { impuesto = 0 }--los {} son para actualizar el campo impuesto del evento, si no es compra se asegura que quede en 0

    esCompra evento = categoria evento == Compra

    calcularImpuesto evento = redondear (valor evento * 0.13)

-- Nombre: reporteImpuestos
-- Descripción: Genera un reporte con el total de eventos, cantidad de eventos tipo COMPRA y eventos con impuesto aplicado.
-- Entradas: Lista de eventos.
-- Salidas: IO () con impresión del reporte en consola.
-- Validaciones: Se asume que los eventos ya están correctamente cargados.
reporteImpuestos :: [Evento] -> IO ()
reporteImpuestos eventos = do

    let eventosConImpuesto = aplicarImpuestos eventos

        totalEventos = length eventos

        totalEventosCompra = length (filter (\evento -> categoria evento == Compra) eventos)

        esCompraConImpuesto evento = categoria evento == Compra && impuesto evento > 0

        totalImpuestosAplicados = length (filter esCompraConImpuesto eventosConImpuesto)

    putStrLn "=================================="
    putStrLn "   REPORTE: IMPUESTOS"
    putStrLn "=================================="
    putStrLn ("Total eventos: " ++ show totalEventos)
    putStrLn ("Eventos tipo COMPRA: " ++ show totalEventosCompra)
    putStrLn ("Eventos con impuesto aplicado: " ++ show totalImpuestosAplicados)
    putStrLn "=================================="


-- Nombre: etiquetarAltoValor
-- Descripción: Marca como True los eventos cuyo valor está por encima del promedio de su categoría.
-- Entradas: Lista de eventos.
-- Salidas: Lista de eventos con el campo etiqueta actualizado.
-- Validaciones: Si una categoría no tiene promedio calculado, la etiqueta se asigna como False.
etiquetarAltoValor :: [Evento] -> [Evento]

etiquetarAltoValor eventos = map (etiquetar eventosConPromedios) eventos
    where

    eventosConPromedios = calcularPromedios eventos

    etiquetar promedios evento =

        let promedioCategoria = lookup (categoria evento) promedios
        in case promedioCategoria of
            Just promedio -> evento { etiqueta = esMayorQuePromedio evento promedio }
            Nothing ->evento { etiqueta = False }

    esMayorQuePromedio evento promedio = valor evento > promedio


-- Nombre: reporteEtiquetas
-- Descripción: Genera un reporte de eventos que están por encima del promedio por categoría.
-- Entradas: Lista de eventos.
-- Salidas: IO () con impresión del reporte en consola.
-- Validaciones: Calcula correctamente los eventos sobre el promedio por categoría.
reporteEtiquetas :: [Evento] -> IO ()
reporteEtiquetas eventos = do

    let eventosConEtiqueta = etiquetarAltoValor eventos

    let totalEventosSobrePromedio = length (filter etiqueta eventosConEtiqueta)

    let categoriasUnicas = nub (map categoria eventos)--nub es para obtener solo las categorías sin repetir, se usa para generar el resumen por categoría

    let resumenPorCategoria = map resumen categoriasUnicas--map aplica la función resumen a cada categoría única para obtener el conteo de eventos sobre el promedio por categoría

        resumen categoriaActual =
            let eventosFiltrados = filter (\evento -> categoria evento == categoriaActual && etiqueta evento ) eventosConEtiqueta
            in (categoriaActual, length eventosFiltrados)

    putStrLn "=================================="
    putStrLn "   REPORTE: ETIQUETAS"
    putStrLn "=================================="
    putStrLn ("Total eventos: " ++ show (length eventos))
    putStrLn ("Eventos sobre promedio: " ++ show totalEventosSobrePromedio)

    putStrLn "----------------------------------"
    putStrLn "Por categoría sobre promedio:"

    mapM_ (\(categoriaNombre, cantidad) -> putStrLn (show categoriaNombre ++ " : " ++ show cantidad)) resumenPorCategoria

    putStrLn "=================================="


-- Nombre: calcularPromedios
-- Descripción: Calcula el promedio de valor por cada categoría de eventos.
-- Entradas: Lista de eventos.
-- Salidas: Lista de tuplas (Categoría, promedio).
-- Validaciones: Evita división por cero al asegurar que cada categoría tenga al menos un evento.

-- PROMEDIO = suma de valores / cantidad de eventos de esa categoría
calcularPromedios :: [Evento] -> [(Categoria, Float)]

calcularPromedios eventos =

    let categoriasUnicas = nub (map categoria eventos)

    in map calcularPromedio categoriasUnicas

    where

    calcularPromedio categoriaActual =

        let eventosDeEstaCategoria = filter perteneceACategoria eventos

            perteneceACategoria evento = categoria evento == categoriaActual

            sumaValores = sum (map valor eventosDeEstaCategoria)

            cantidadEventos = length eventosDeEstaCategoria

            promedio = sumaValores / fromIntegral cantidadEventos

        in (categoriaActual, promedio)


-- Nombre: redondear
-- Descripción: Redondea un número flotante a 2 decimales.
-- Entradas: Número Float.
-- Salidas: Float redondeado.
-- Validaciones: No aplica.
redondear :: Float -> Float
redondear numero = fromIntegral (round (numero * 100)) / 100


-- Nombre: reporteCompleto
-- Descripción: Genera un reporte combinado de impuestos y etiquetas.
-- Entradas: Lista de eventos.
-- Salidas: IO () con impresión del reporte en consola.
-- Validaciones: Aplica primero impuestos y luego etiquetas antes de generar el reporte.
reporteCompleto :: [Evento] -> IO ()

reporteCompleto eventos = do

    let eventosConImpuestos = aplicarImpuestos eventos

    let eventosFinales = etiquetarAltoValor eventosConImpuestos

    let totalImpuestos = length (filter (\evento -> categoria evento == Compra && impuesto evento > 0 ) eventosFinales)

    let totalEtiquetas = length (filter etiqueta eventosFinales)

    putStrLn "=================================="
    putStrLn "   REPORTE COMPLETO"
    putStrLn "=================================="
    putStrLn ("Total eventos: " ++ show (length eventosFinales))
    putStrLn ("Impuestos aplicados: " ++ show totalImpuestos)
    putStrLn ("Eventos sobre promedio: " ++ show totalEtiquetas)
    putStrLn "=================================="
