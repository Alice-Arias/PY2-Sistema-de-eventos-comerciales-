
module Types.Evento where 

import Types.Categoria
import Types.MetodoPago
import Types.Estado
import Types.Producto


data Evento = Evento 
    { idEvento :: Int
    , categoria :: Categoria
    , valor :: Float
    , timestamp :: Int
    , usuarioId :: String
    , productoId :: String
    , producto :: Producto
    , cantidad :: Int
    , metodoPago :: MetodoPago
    , estado :: Estado
    , impuesto :: Float
    , etiqueta :: Bool
    , total :: Float
    } deriving (Show, Read, Eq)