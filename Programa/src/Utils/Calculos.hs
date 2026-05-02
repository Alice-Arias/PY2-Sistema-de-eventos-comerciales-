module Utils.Calculos where

import Types.Evento
import Types.Categoria


calcularSubtotal :: Float -> Int -> Float
calcularSubtotal precioUnitario cantidad =
    precioUnitario * fromIntegral cantidad


calcularImpuesto13 :: Float -> Float
calcularImpuesto13 subtotal = subtotal * 0.13


redondear :: Float -> Float
redondear valor = fromIntegral (round (valor * 100)) / 100



calcularTotales :: Evento -> Evento
calcularTotales evento =

    let precioUnitario = valor evento
        cantidadUnidades = cantidad evento

        subtotalCalculado = calcularSubtotal precioUnitario cantidadUnidades

        subtotalRedondeado = redondear subtotalCalculado

    in case categoria evento of

        Compra ->
            let impuestoCalculado = redondear (calcularImpuesto13 subtotalRedondeado)

                totalFinal = subtotalRedondeado + impuestoCalculado

            in evento{ impuesto = impuestoCalculado, total = totalFinal }
        _ ->
            evento{ impuesto = 0, total = subtotalRedondeado }



actualizarTotales :: [Evento] -> [Evento]
actualizarTotales = map calcularTotales



porcentaje :: Float -> Float -> Float
porcentaje parte total = (parte / total) * 100