module Types.Categoria where

-- categoria de eventos de tipo algebrico, para poder categorizar los eventos de la aplicacion
data Categoria = Visualizacion | Apartado | Compra | Devolucion | Seguimiento deriving (Show, Read, Eq)