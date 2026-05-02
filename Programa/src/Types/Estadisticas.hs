module Types.Estadisticas where

data Estadistica = Estadistica
    { estId :: Int
    , fechaConsulta :: Int
    , eventosPorCategoria :: String
    , eventoMaximo :: String
    , eventoMinimo :: String
    , diaMasActivo :: String
    } deriving (Show, Read)