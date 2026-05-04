module Core.Impuestos where

import Types.Modelos
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: aplicarImpuestos
-- Entrada: eventos: lista de registros del sistema con todas las acciones realizadas
-- Salida: lista de eventos donde a las compras se les calcula impuesto y total final
-- Restricciones:
--   Solo aplica impuestos a eventos de tipo compra
--   Los eventos ya con impuesto no se vuelven a recalcular
--------------------------------------------------------------------------------
aplicarImpuestos :: [Evento] -> [Evento]
aplicarImpuestos = map aplicarImpuestoEvento

--------------------------------------------------------------------------------
-- Nombre: promedioCompras
-- Entrada: eventos: lista de registros del sistema
-- Salida: promedio del valor total de todas las compras
-- Restricciones:
--   Si no hay compras, el resultado puede ser 0 o indefinido según el cálculo interno
--------------------------------------------------------------------------------
promedioCompras :: [Evento] -> Float
promedioCompras eventos =
    let 
        compras = filtrarCompras eventos
        sumaTotal = calcularSumaTotales compras
        totalCompras = contarEventos compras

    in 
        calcularPromedioLocal sumaTotal totalCompras

--------------------------------------------------------------------------------
-- Nombre: aplicarImpuestoEvento
-- Entrada: evento: registro individual del sistema
-- Salida:
--   mismo evento actualizado con impuesto y total si es compra
--   si no es compra, el evento queda igual
-- Restricciones:
--   no recalcula impuestos si ya existen
--------------------------------------------------------------------------------
aplicarImpuestoEvento :: Evento -> Evento
aplicarImpuestoEvento evento

    | yaTieneImpuesto evento = evento

    | esCompra evento        = calcularImpuestoCompra evento

    | otherwise              = evento { impuesto = 0 }

--------------------------------------------------------------------------------
-- Nombre: calcularImpuestoCompra
-- Entrada: evento de tipo compra
-- Salida:
--   evento actualizado con impuesto calculado y total final con impuesto incluido
-- Restricciones:
--   solo se usa para eventos de compra
--------------------------------------------------------------------------------
calcularImpuestoCompra :: Evento -> Evento
calcularImpuestoCompra evento =

    let 
        subtotal = calcularSubtotal (valor evento) (cantidad evento)
        impuestoCalculado = redondear (calcularImpuesto13 subtotal)
        totalFinal = redondear (subtotal + impuestoCalculado)

    in 
        evento { impuesto = impuestoCalculado, total = totalFinal }

--------------------------------------------------------------------------------
-- Nombre: yaTieneImpuesto
-- Entrada: evento individual
-- Salida: verdadero si el evento ya tiene impuesto aplicado
-- Restricciones: ninguna
--------------------------------------------------------------------------------
yaTieneImpuesto :: Evento -> Bool
yaTieneImpuesto evento = impuesto evento /= 0

--------------------------------------------------------------------------------
-- Nombre: esCompra
-- Entrada: evento individual
-- Salida: verdadero si el evento es de tipo compra
-- Restricciones: depende de la categoría del evento
--------------------------------------------------------------------------------
esCompra :: Evento -> Bool
esCompra evento = categoria evento == Compra

--------------------------------------------------------------------------------
-- Nombre: filtrarCompras
-- Entrada: lista de eventos
-- Salida: solo los eventos que son compras
-- Restricciones: ninguna
--------------------------------------------------------------------------------
filtrarCompras :: [Evento] -> [Evento]
filtrarCompras = filter (\e -> categoria e == Compra)