module Types.Estado where

-- Nombre: Estado
-- Descripcion: Tipo de dato que representa el estado de un evento.
-- Entradas: No recibe parámetros. Solo puede tomar uno de estos valores: Completado, Pendiente, Cancelado.
-- Salidas:Un valor del tipo Estado.
-- Validaciones: Solo permite los valores definidos en el tipo de dato.
data Estado = Completado | Pendiente | Cancelado deriving (Show, Read, Eq)