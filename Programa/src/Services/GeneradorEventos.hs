module Services.GeneradorEventos where

import Data.Time
import Types.Evento
import Types.Categoria
import Types.MetodoPago
import Types.Estado
import Types.Producto

categorias :: [Categoria]
categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]

metodosPago :: [MetodoPago]
metodosPago = [Tarjeta, Sinpe, Efectivo]

estadosEvento :: [Estado]
estadosEvento = [Pendiente, Completado, Cancelado]

productos :: [Producto]
productos =
    [ "Laptop", "Celular", "Tablet", "Audifonos", "Teclado", "Mouse"
    , "Monitor", "Impresora", "Camara", "Consola", "Smartwatch"
    , "Bocina", "Microfono", "Router", "DiscoDuro", "SSD"
    , "USB", "Cargador", "Proyector", "TV", "Silla Gamer"]

limitarCantidad :: Int -> Int
limitarCantidad n
    | n < 10 = 10
    | n > 25 = 25
    | otherwise = n


obtenerFechaActual :: IO Day
obtenerFechaActual = utctDay <$> getCurrentTime

generarFechaEvento :: Day -> Int -> IO Int
generarFechaEvento fechaBase indice = do
    tiempoActual <- getCurrentTime

    let
        segundos = floor (utctDayTime tiempoActual * 1000) :: Int -- Obtener milisegundos para generar ruido
        ruido = segundos `mod` 997
        desplazamiento =(indice * 41 + indice * indice + ruido * 17) `mod` 730
        nuevaFecha = addDays (fromIntegral desplazamiento) fechaBase
        (anio, mes, dia) = toGregorian nuevaFecha

    return (fromIntegral anio * 10000 + mes * 100 + dia)


productosConCodigo :: [(String, Producto)]
productosConCodigo =zip (map (\i -> "P" ++ show (200 + i)) [0..]) productos

usuariosConCodigo :: [String]
usuariosConCodigo = map (\i -> "U" ++ show (300 + i)) [0..299]


generarEventos :: [Evento] -> IO [Evento]
generarEventos eventosExistentes = do
    fechaBase <- obtenerFechaActual

    let
        cantidadBase = length eventosExistentes
        cantidadEventos = limitarCantidad (10 + (cantidadBase `div` 5) `mod` 16)
        indices = [cantidadBase + 1 .. cantidadBase + cantidadEventos]

    mapM (crearEvento fechaBase) indices

crearEvento :: Day -> Int -> IO Evento
crearEvento fechaBase indice = do
    fecha <- generarFechaEvento fechaBase indice
    tiempo <- getCurrentTime

    let ruido = floor (utctDayTime tiempo * 1000) :: Int
        idEvento = indice - 1

        mezclaCat = indice * 131+ ruido * 97 + idEvento * 53+ (indice `div` 3) * 17

        idxCategoria = abs mezclaCat `mod` 100 -- para distribuir entre 5 categorías de forma no lineal

        categoria 
            | idxCategoria < 20 = Visualizacion
            | idxCategoria < 40 = Apartado
            | idxCategoria < 70 = Compra
            | idxCategoria < 85 = Seguimiento
            | otherwise = Devolucion


        (idProducto, nombreProducto) = productosConCodigo !! (indice `mod` length productosConCodigo)

        usuario = usuariosConCodigo !! (indice `mod` length usuariosConCodigo)

        metodo = metodosPago !! (indice `mod` length metodosPago)

        estado =estadosEvento !! (indice `mod` length estadosEvento)

        valor = calcularValor idEvento indice ruido

        cantidad = calcularCantidad categoria indice

        subtotal = valor * fromIntegral cantidad

        total =
            if categoria == Compra
                then 0
                else subtotal

    return (Evento
        idEvento
        categoria
        valor
        fecha
        usuario
        idProducto
        nombreProducto
        cantidad
        metodo
        estado
        0
        False
        total)

calcularValor :: Int -> Int -> Int -> Float
calcularValor idEvento indice ruido =
    let 
        mezcla = idEvento * 131 + indice * 97 + ruido * 19
        rango = mezcla `mod` 74500
    in 
        fromIntegral (max 500 (500 + rango))


calcularCantidad :: Categoria -> Int -> Int
calcularCantidad cat indice =
    case cat of
        Visualizacion -> 1
        Seguimiento   -> 1
        Apartado      -> 1 + (indice `mod` 3)
        Compra        -> 1 + (indice `mod` 8)
        Devolucion    -> 1


obtenerUltimoId :: [Evento] -> Int
obtenerUltimoId [] = -1
obtenerUltimoId eventos = maximum (map idEvento eventos)