-- Nombre: Types.Evento
-- Descripcion: Módulo que define el tipo de dato Evento y usa otros tipos relacionados.
-- Entradas: No recibe datos directamente. Usa tipos: Categoria, MetodoPago, Estado y Producto.
-- Salidas:Expone el tipo Evento para usarlo en otros módulos.
-- Validaciones: Solo valida tipos (Int, Float, etc.). No valida valores específicos ni consistencia entre campos.

module Types.Evento where 

import Types.Categoria
import Types.MetodoPago
import Types.Estado
import Types.Producto

-- Nombre: Evento

-- Descripción: Representa una transacción o interacción dentro del sistema.
-- Cada evento guarda información sobre qué ocurrió, con qué producto, qué usuario participó
-- y cómo se procesó (pago, estado, impuesto, etc.).

-- Entradas (al crear un Evento):

--  idEvento: identificador único del evento
--  categoria: tipo de acción realizada (Visualizacion, Apartado, Compra, Devolucion, Seguimiento)
--  valor: monto económico asociado al evento (rango aproximado 500 a 75000)
--  timestamp: fecha del evento en formato numérico YYYYMMDD
--  usuarioId: usuario que realizó el evento (ej: U300 a U599)
--  productoId: identificador del producto (ej: P200 a P219)
--  producto: nombre del producto involucrado
--  cantidad: unidades involucradas (depende de la categoría, normalmente 1 a 8)
--  metodoPago: forma de pago utilizada (Tarjeta, Efectivo, Sinpe)
--  estado: estado del proceso del evento (Pendiente, Completado, Cancelado)
--  impuesto: impuesto calculado según la categoría del evento
--  etiqueta: indicador de si el evento es de alto valor
-- Salida: Un valor del tipo Evento completamente construido.
-- Validaciones: Se asume que los tipos son correctos.
-- No valida reglas de negocio complejas como consistencia entre campos o valores negativos.
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