module Core.Impuestos where

import Types.Evento
import Types.Categoria
import Utils.Calculos

aplicarImpuestos :: [Evento] -> [Evento]
aplicarImpuestos = map aplicar
    where
        aplicar evento

            | impuesto evento /= 0 = evento

            | categoria evento == Compra = -- aplicar impuesto
                let 
                    subtotal = calcularSubtotal (valor evento) (cantidad evento)
                    imp = calcularImpuesto13 subtotal
                in 
                    evento{ impuesto = redondear imp, total = redondear (subtotal + imp)}

            | otherwise = evento { impuesto = 0 }


promedioCompras :: [Evento] -> Float

promedioCompras listaEventos =
    
    let 
        comprasFiltradas = filter (\evento -> categoria evento == Compra) listaEventos

        sumaTotales = sum (map total comprasFiltradas)

        cantidadCompras = length comprasFiltradas

    in 
        if cantidadCompras == 0
        then 0
        else sumaTotales / fromIntegral cantidadCompras