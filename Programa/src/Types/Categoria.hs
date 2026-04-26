module Types.Categoria where

-- Nombre: Categoria
-- Descripcion: Tipo de dato que representa la categoría de un evento.
-- Entradas:No recibe parámetros al crearse, solo puede tomar uno de estos valores: Visualizacion, Apartado, Compra, Devolucion, Seguimiento.
-- Salidas:Un valor del tipo Categoria.
-- Validaciones: Solo permite los valores definidos en el tipo de dato.
data Categoria = Visualizacion | Apartado | Compra | Devolucion | Seguimiento deriving (Show, Read, Eq)