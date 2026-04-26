module Types.MetodoPago where

-- Nombre: MetodoPago
-- Descripcion: Tipo de dato que representa la forma de pago de un evento.
-- Entradas: No recibe parámetros. Solo puede tomar uno de estos valores: Tarjeta, Efectivo, Sinpe.
-- Salidas: Un valor del tipo MetodoPago.
-- Validdaciones: Solo permite los valores definidos en el tipo de dato.
data MetodoPago = Tarjeta | Efectivo | Sinpe deriving (Show, Read, Eq)