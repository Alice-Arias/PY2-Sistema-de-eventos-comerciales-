module Types.Evento where

import Types.Categoria
import Types.MetodoPago
import Types.Estado
import Types.Producto

data Evento = Evento { idE :: Int , categoria :: Categoria , valor :: Float, timestamp :: Int, usuarioId :: Int, productoId :: Int, producto :: Producto, cantidad :: Int, metodoPago :: MetodoPago, estado :: Estado, impuesto :: Float} deriving (Show, Read, Eq)