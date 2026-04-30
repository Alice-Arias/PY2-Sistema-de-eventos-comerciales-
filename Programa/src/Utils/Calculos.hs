module Utils.Calculos where

import Types.Evento
import Types.Categoria

calcularSubtotal :: Float -> Int -> Float
calcularSubtotal precio cantidad =
    precio * fromIntegral cantidad


calcularImpuesto13 :: Float -> Float
calcularImpuesto13 subtotal =
    subtotal * 0.13


redondear :: Float -> Float
redondear numero =
    fromIntegral (round (numero * 100)) / 100


-- =========================
-- TRANSFORMACIÓN DE EVENTOS
-- =========================

calcularTotales :: Evento -> Evento
calcularTotales evento =

    let subtotal = calcularSubtotal (valor evento) (cantidad evento)
        subtotalRedondeado = redondear subtotal

    in case categoria evento of

        Compra ->
            let impuesto = redondear (calcularImpuesto13 subtotalRedondeado)
            in evento { impuesto = impuesto, total = subtotalRedondeado + impuesto }

        _ ->
            evento { impuesto = 0 , total = subtotalRedondeado }


actualizarTotales :: [Evento] -> [Evento]
actualizarTotales =
    map calcularTotales


porcentaje :: Float -> Float -> Float
porcentaje parte total =
    (parte / total) * 100