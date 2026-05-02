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
    , "USB", "Cargador", "Proyector", "TV", "Silla Gamer"
    ]


limitarCantidad :: Int -> Int
limitarCantidad n
    | n < 10 = 10
    | n > 25 = 25
    | otherwise = n


obtenerFechaActual :: IO Day
obtenerFechaActual = utctDay <$> getCurrentTime


obtenerMilisegundosActuales :: IO Int
obtenerMilisegundosActuales = do
    tiempo <- getCurrentTime
    return (floor (utctDayTime tiempo * 1000) :: Int)


obtenerFechaEvento :: Day -> Int -> IO Int
obtenerFechaEvento fechaBase indice = do
    milisegundos <- obtenerMilisegundosActuales

    let ruido = milisegundos `mod` 997
        desplazamiento = (indice * 41 + indice * indice + ruido * 17) `mod` 730

        nuevaFecha = addDays (fromIntegral desplazamiento) fechaBase

        (anio, mes, dia) = toGregorian nuevaFecha

    return (fromIntegral anio * 10000 + mes * 100 + dia)



productosConCodigo :: [(String, Producto)]
productosConCodigo = zip (map (\i -> "P" ++ show (200 + i)) [0..]) productos


usuariosConCodigo :: [String]
usuariosConCodigo = map (\i -> "U" ++ show (300 + i)) [0..299]


obtenerProducto :: Int -> (String, Producto)
obtenerProducto indice = productosConCodigo !! (indice `mod` length productosConCodigo)


obtenerUsuario :: Int -> String
obtenerUsuario indice = usuariosConCodigo !! (indice `mod` length usuariosConCodigo)


generarEventos :: [Evento] -> IO [Evento]
generarEventos eventosExistentes = do
    fechaBase <- obtenerFechaActual

    let cantidadBase = length eventosExistentes
        cantidadEventos = limitarCantidad (10 + (cantidadBase `div` 5) `mod` 16)

        indices = [cantidadBase + 1 .. cantidadBase + cantidadEventos]

    mapM (crearEvento fechaBase) indices


crearEvento :: Day -> Int -> IO Evento
crearEvento fechaBase indice = do
    fecha <- obtenerFechaEvento fechaBase indice
    milisegundos <- obtenerMilisegundosActuales

    let idEvento = indice - 1

        categoriaEvento = determinarCategoria indice milisegundos idEvento

        (idProducto, nombreProducto) = obtenerProducto indice
        usuario = obtenerUsuario indice

        metodo = seleccionarMetodo indice
        estado = seleccionarEstado indice

        valor = calcularValor idEvento indice milisegundos
        cantidad = calcularCantidad categoriaEvento indice

        subtotal = valor * fromIntegral cantidad

        totalFinal =
            if categoriaEvento == Compra
            then 0
            else subtotal

    return (Evento
        idEvento
        categoriaEvento
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
        totalFinal)

determinarCategoria :: Int -> Int -> Int -> Categoria
determinarCategoria indice ruido idEvento =
    let mezcla =
            indice * 131 + ruido * 97 + idEvento * 53 + (indice `div` 3) * 17

        idx = abs mezcla `mod` 100

    in
        if idx < 20 then Visualizacion
        else if idx < 40 then Apartado
        else if idx < 70 then Compra
        else if idx < 85 then Seguimiento
        else Devolucion


seleccionarMetodo :: Int -> MetodoPago
seleccionarMetodo indice =
    metodosPago !! (indice `mod` length metodosPago)


seleccionarEstado :: Int -> Estado
seleccionarEstado indice =
    estadosEvento !! (indice `mod` length estadosEvento)


calcularValor :: Int -> Int -> Int -> Float
calcularValor idEvento indice ruido =
    let mezcla = idEvento * 131 + indice * 97 + ruido * 19
        rango = mezcla `mod` 74500
    in fromIntegral (max 500 (500 + rango))


calcularCantidad :: Categoria -> Int -> Int
calcularCantidad categoria indice =
    case categoria of
        Visualizacion -> 1
        Seguimiento   -> 1
        Apartado      -> 1
        Compra        -> 1 + (indice `mod` 3)
        Devolucion    -> 1


obtenerUltimoId :: [Evento] -> Int
obtenerUltimoId eventos =
    if null eventos
        then -1
        else maximum (map idEvento eventos)