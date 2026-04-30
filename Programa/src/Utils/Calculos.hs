module Utils.Calculos where

import Types.Evento
import Types.Categoria

calcularSubtotal :: Float -> Int -> Float
calcularSubtotal precioUnitario cantidad = precioUnitario * fromIntegral cantidad


calcularImpuesto13 :: Float -> Float
calcularImpuesto13 subtotal = subtotal * 0.13


redondear :: Float -> Float
redondear numero = fromIntegral (round (numero * 100)) / 100


calcularTotales :: Evento -> Evento
calcularTotales evento =
    let
        subtotal = calcularSubtotal (valor evento) (cantidad evento)
        subtotalR = redondear subtotal
    in
        case categoria evento of
            Compra ->
                let 
                    impuesto = redondear (calcularImpuesto13 subtotalR)
                in 
                    evento{ impuesto = impuesto, total = subtotalR + impuesto}
            _ ->
                evento{ impuesto = 0, total = subtotalR}


actualizarTotales :: [Evento] -> [Evento]
actualizarTotales = map calcularTotales