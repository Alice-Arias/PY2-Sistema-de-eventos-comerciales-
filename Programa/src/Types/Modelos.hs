module Types.Modelos where


--------------------------------------------------------------------------------
-- Nombre: Categoria
-- Entrada: ninguna (tipo enumerado)
-- Salida: representa el tipo de acción de un evento
-- Restricciones:
--   - Solo puede tomar los valores definidos en la lista
--------------------------------------------------------------------------------
data Categoria = Visualizacion
    | Apartado
    | Compra
    | Devolucion
    | Seguimiento
    deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Nombre: Estadistica
-- Entrada: datos generados por el sistema de análisis
-- Salida: estructura con resumen general del sistema
-- Restricciones:
--   - Se usa solo para reportes, no para lógica de negocio directa
--------------------------------------------------------------------------------
data Estadistica = Estadistica
    { estId               :: Int
    , fechaConsulta       :: Int
    , eventosPorCategoria :: String
    , eventoMaximo        :: String
    , eventoMinimo        :: String
    , diaMasActivo        :: String
    } deriving (Show, Read)

--------------------------------------------------------------------------------
-- Nombre: Estado
-- Entrada: ninguna (tipo enumerado)
-- Salida: estado actual de un evento
-- Restricciones:
--   - Solo puede ser Completado, Pendiente o Cancelado
--------------------------------------------------------------------------------
data Estado = Completado
    | Pendiente
    | Cancelado
    deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Nombre: Evento
-- Entrada: datos generados o ingresados al sistema
-- Salida: representa un registro completo del sistema
-- Restricciones:
--   - Todos los campos deben estar correctamente inicializados
--------------------------------------------------------------------------------
data Evento = Evento
    { idEvento       :: Int
    , categoria      :: Categoria
    , valor          :: Float
    , timestamp      :: Int
    , usuarioId      :: String
    , productoId     :: String
    , producto       :: Producto
    , cantidad       :: Int
    , metodoPago     :: MetodoPago
    , estado         :: Estado
    , impuesto       :: Float
    , etiqueta       :: Bool
    , total          :: Float
    } deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Nombre: MetodoPago
-- Entrada: ninguna (tipo enumerado)
-- Salida: forma de pago usada en el sistema
-- Restricciones:
--   - Solo permite los valores definidos
--------------------------------------------------------------------------------
data MetodoPago = Tarjeta
    | Efectivo
    | Sinpe
    deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Nombre: Producto
-- Entrada: ninguna (alias de tipo)
-- Salida: nombre del producto como texto
-- Restricciones:
--   - Se representa únicamente como String
--------------------------------------------------------------------------------
type Producto = String