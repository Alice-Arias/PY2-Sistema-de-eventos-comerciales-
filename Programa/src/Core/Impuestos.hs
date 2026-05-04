module Core.Impuestos where

import Types.Modelos
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: aplicarImpuestos
--
-- Objetivo: aplica el cálculo de impuestos a todos los eventos de tipo compra
--
-- Entradas: lista de eventos del sistema
--
-- Salida: lista de eventos con impuestos y totales actualizados
--
-- Restricciones:
--   - Solo se modifica eventos de tipo Compra
--   - Los eventos que ya tienen impuesto no se recalculan
--------------------------------------------------------------------------------
aplicarImpuestos :: [Evento] -> [Evento]
aplicarImpuestos = map aplicarImpuestoEvento

--------------------------------------------------------------------------------
-- Nombre: promedioCompras
--
-- Objetivo: calcula el promedio del total de todas las compras registradas
--
-- Entradas: lista de eventos
--
-- Salida: promedio de los montos de compras
--
-- Restricciones:
--   - Si no hay compras, el resultado es 0
--------------------------------------------------------------------------------
promedioCompras :: [Evento] -> Float
promedioCompras eventos =
    let
        compras = filtrarCompras eventos
        totalMonto = calcularSumaTotales compras
        cantidadCompras = contarEventos compras
    in
        calcularPromedioLocal totalMonto cantidadCompras

--------------------------------------------------------------------------------
-- Nombre: aplicarImpuestoEvento
--
-- Objetivo: aplica impuestos a un evento si corresponde
--
-- Entradas: evento individual
--
-- Salida: evento actualizado con impuesto y total, o sin cambios
--
-- Restricciones:
--   - Si el evento ya tiene impuesto, no se recalcula
--------------------------------------------------------------------------------
aplicarImpuestoEvento :: Evento -> Evento
aplicarImpuestoEvento evento
    | yaTieneImpuesto evento =
                                evento

    | esCompra evento =
                        calcularImpuestoCompra evento

    | otherwise =
                    evento { impuesto = 0 }

--------------------------------------------------------------------------------
-- Nombre: calcularImpuestoCompra
--
-- Objetivo: calcula el impuesto y total final para un evento de compra
--
-- Entradas: evento de tipo Compra
--
-- Salida: evento con impuesto y total actualizados
--
-- Restricciones:
--   - Solo aplica a eventos de compra
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
--
-- Objetivo: verifica si un evento ya tiene impuesto aplicado
--
-- Entradas: evento individual
--
-- Salida: True si ya tiene impuesto, False si no
--
-- Restricciones: ninguna
--------------------------------------------------------------------------------
yaTieneImpuesto :: Evento -> Bool
yaTieneImpuesto evento = impuesto evento /= 0

--------------------------------------------------------------------------------
-- Nombre: esCompra
--
-- Objetivo: verifica si un evento es de tipo compra
--
-- Entradas: evento individual
--
-- Salida: True si es compra
--
-- Restricciones: depende del campo categoría
--------------------------------------------------------------------------------
esCompra :: Evento -> Bool
esCompra evento = categoria evento == Compra

--------------------------------------------------------------------------------
-- Nombre: filtrarCompras
--
-- Objetivo: obtiene únicamente los eventos de tipo compra
--
-- Entradas: lista de eventos
--
-- Salida: lista filtrada de compras
--
-- Restricciones: ninguna
--------------------------------------------------------------------------------
filtrarCompras :: [Evento] -> [Evento]
filtrarCompras = filter esCompra