module Core.Impuestos where

import Types.Evento
import Types.Categoria
import Utils.Calculos


aplicarImpuestos :: [Evento] -> [Evento]
aplicarImpuestos = map aplicarImpuestoEvento

aplicarImpuestoEvento :: Evento -> Evento
aplicarImpuestoEvento evento

    -- Si ya tiene impuesto, no hacer nada
    | yaTieneImpuesto evento = evento

    -- Si es compra, calcular impuesto
    | esCompra evento = calcularImpuestoCompra evento

    | otherwise = evento { impuesto = 0 }


yaTieneImpuesto :: Evento -> Bool
yaTieneImpuesto evento = impuesto evento /= 0

esCompra :: Evento -> Bool
esCompra evento = categoria evento == Compra


calcularImpuestoCompra :: Evento -> Evento
calcularImpuestoCompra evento =
    let
        valorUnitario = valor evento

        cantidadProducto = cantidad evento

        subtotal = calcularSubtotal valorUnitario cantidadProducto

        impuestoCalculado = calcularImpuesto13 subtotal

        impuestoFinal = redondear impuestoCalculado

        totalFinal = redondear (subtotal + impuestoCalculado)

    in
        evento { impuesto = impuestoFinal , total = totalFinal }


promedioCompras :: [Evento] -> Float
promedioCompras listaEventos =

    let
        eventosCompra = filtrarCompras listaEventos

        sumaTotales = calcularSumaTotales eventosCompra

        cantidadCompras = contarEventos eventosCompra

    in
        calcularPromedio sumaTotales cantidadCompras



filtrarCompras :: [Evento] -> [Evento]
filtrarCompras = filter (\evento -> categoria evento == Compra)


calcularSumaTotales :: [Evento] -> Float
calcularSumaTotales eventos =
    sum (map total eventos)


contarEventos :: [Evento] -> Int
contarEventos = length


calcularPromedio :: Float -> Int -> Float
calcularPromedio suma cantidad =
    if cantidad == 0
    then 0
    else suma / fromIntegral cantidad