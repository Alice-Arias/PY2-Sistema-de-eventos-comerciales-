module Services.GeneradorEventos where

import Data.Time
import Types.Evento
import Types.Categoria
import Types.MetodoPago
import Types.Estado
import Types.Producto



categorias :: [Categoria]
categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]

metodos :: [MetodoPago]
metodos = [Tarjeta, Sinpe, Efectivo]

estados :: [Estado]
estados = [Pendiente, Completado, Cancelado]

productos :: [Producto]
productos =[ "Laptop", "Celular", "Tablet", "Audifonos", "Teclado", "Mouse", "Monitor", "Impresora", "Camara", "Consola", "Smartwatch", "Bocina", "Microfono", "Router", "DiscoDuro", "SSD", "USB", "Cargador", "Proyector", "TV", "Ventilador", "AireAcondicionado", "Refrigeradora", "Microondas", "Horno", "Cafetera", "Licuadora", "Plancha", "Aspiradora", "Secadora", "Lavadora", "Silla", "Escritorio", "Lampara", "Mesa", "Sofa", "Cama", "Colchon", "Almohada", "Cobija", "Libro", "Cuaderno", "Lapiz", "Boligrafo", "Mochila", "Calculadora", "Reloj", "Gafas", "Bateria", "Adaptador"]


limitar :: Int -> Int
limitar n
    | n < 10 = 10
    | n > 25 = 25
    | otherwise = n


obtenerUltimoId :: [Evento] -> Int
obtenerUltimoId [] = 1000
obtenerUltimoId eventos = maximum (map idE eventos)

obtenerFechaActual :: IO Day
obtenerFechaActual = utctDay <$> getCurrentTime

generarFecha :: Day -> Int -> Int
generarFecha fechaBase i =
    let nuevaFecha = addDays (fromIntegral (i `mod` 730)) fechaBase
        (year, month, day) = toGregorian nuevaFecha
    in fromIntegral year * 10000 + month * 100 + day


generarEventos :: Int -> [Evento] -> IO [Evento]
generarEventos n existentes = do
    fechaBase <- obtenerFechaActual
    let base = obtenerUltimoId existentes
        cantidad = limitar n
    return (map (crearEventoDesde base fechaBase) [1..cantidad])


crearEventoDesde :: Int -> Day -> Int -> Evento
crearEventoDesde base fechaBase i =
    let idx = base + i
        cat = categorias !! (idx `mod` length categorias)
        val = fromIntegral (500 + ((idx * 100) `mod` 74500))
        fecha = generarFecha fechaBase idx
    in Evento
        (idx `mod` 9000000)
        cat
    val
    fecha
    (10 + idx)
    (200 + idx)
    (productos !! (idx `mod` length productos))
    ((idx `mod` 10) + 1)
    (metodos !! (idx `mod` length metodos))
    (estados !! (idx `mod` length estados))
    (calcularImpuesto cat val)


calcularImpuesto :: Categoria -> Float -> Float
calcularImpuesto cat val =
    if cat == Compra
    then redondear (val * 0.13)
    else 0

redondear :: Float -> Float
redondear x = fromIntegral (round (x * 100)) / 100