module Services.GeneradorEventos where

import Data.Time
import Types.Evento
import Types.Categoria
import Types.MetodoPago
import Types.Estado
import Types.Producto

-- =========================
-- BASE DE DATOS PARA GENERAR
-- =========================

-- Nombre: categorias
-- Descripción: Lista de categorías disponibles para clasificar los eventos del sistema.
-- Entradas: No recibe parámetros.
-- Salidas: Lista de valores del tipo Categoria.
-- Validaciones: Solo contiene valores definidos en el tipo Categoria.
categorias :: [Categoria]
categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]

-- Nombre: metodos
-- Descripción: Lista de métodos de pago disponibles en el sistema.
-- Entradas: No recibe parámetros.
-- Salidas: Lista de valores del tipo MetodoPago.
-- Validaciones: Solo contiene valores válidos definidos en MetodoPago.
metodos :: [MetodoPago]
metodos = [Tarjeta, Sinpe, Efectivo]

-- Nombre: estados
-- Descripción: Lista de estados posibles para un evento.
-- Entradas: No recibe parámetros.
-- Salidas: Lista de valores del tipo Estado.
-- Validaciones: Solo contiene estados definidos en el sistema.
estados :: [Estado]
estados = [Pendiente, Completado, Cancelado]

-- Nombre: productos
-- Descripción: Lista de productos disponibles en el sistema.
-- Entradas: No recibe parámetros.
-- Salidas: Lista de productos tipo String.
-- Validaciones: Contiene únicamente productos válidos.
productos :: [Producto]
productos =
    [ "Laptop", "Celular", "Tablet", "Audifonos", "Teclado", "Mouse"
    , "Monitor", "Impresora", "Camara", "Consola", "Smartwatch"
    , "Bocina", "Microfono", "Router", "DiscoDuro", "SSD"
    , "USB", "Cargador", "Proyector", "TV"
    ]


-- Nombre: limitar
-- Descripción: Ajusta la cantidad de eventos a generar dentro de un rango permitido.
-- Entradas: Int n (cantidad solicitada).
-- Salidas: Int ajustado entre 10 y 25.
-- Validaciones: Si n < 10 devuelve 10, si n > 25 devuelve 25, si está en rango lo mantiene.
limitar :: Int -> Int

limitar n
    | n < 10 = 10
    | n > 25 = 25
    | otherwise = n


-- Nombre: obtenerFechaActual
-- Descripción: Obtiene la fecha actual del sistema.
-- Entradas: No recibe parámetros.
-- Salidas: IO Day con la fecha actual.
-- Validaciones: Depende del sistema operativo.
obtenerFechaActual :: IO Day

obtenerFechaActual = utctDay <$> getCurrentTime

-- Nombre: generarFecha
-- Descripción: Genera una fecha a partir de una fecha base y un índice.
-- Entradas: Day fechaBase, Int indice.
-- Salidas: Int en formato YYYYMMDD.
-- Validaciones: Genera fechas dentro de un rango aproximado de 2 años.
generarFecha :: Day -> Int -> Int

generarFecha fechaBase indice =

    let fechaNueva = addDays (fromIntegral (indice `mod` 730)) fechaBase
        (anio, mes, dia) = toGregorian fechaNueva

    in fromIntegral anio * 10000 + mes * 100 + dia


-- Nombre: productosConId
-- Descripción: Asocia cada producto con un identificador único.
-- Entradas: No recibe parámetros.
-- Salidas: Lista de tuplas (String, Producto).
-- Validaciones: IDs generados de forma secuencial.
productosConId :: [(String, Producto)]

productosConId =
    zip (map (\i -> "P" ++ show (200 + i)) [0..length productos - 1]) productos --zip hace id y producto juntos osea como una tupla de pares, ejemplo: [("P200", "Laptop"), ("P201", "Celular"), etc.]

-- Nombre: usuariosConId
-- Descripción: Genera lista de usuarios del sistema con IDs únicos.
-- Entradas: No recibe parámetros.
-- Salidas: Lista de Strings con IDs de usuario.
-- Validaciones: Genera 300 usuarios consecutivos.
usuariosConId :: [String]

usuariosConId =
    map (\i -> "U" ++ show (300 + i)) [0..299]

-- =========================
-- GENERADOR PRINCIPAL
-- =========================

-- Nombre: generarEventos
-- Descripción: Genera eventos nuevos basados en eventos existentes.
-- Entradas: Lista de Evento existentes.
-- Salidas: IO [Evento].
-- Validaciones: Calcula cantidad de eventos entre 10 y 25 según tamaño del sistema.
generarEventos :: [Evento] -> IO [Evento]

generarEventos eventosExistentes = do
    fechaBase <- obtenerFechaActual

    let idBase = obtenerUltimoId eventosExistentes

        base = length eventosExistentes

        cantidadElementosCreados = 10 + (base `div` 5) `mod` 16-- Esto genera un número entre 10 y 25 basado en la cantidad de eventos existentes, aumentando  a medida que el sistema crece.

        cantidadFinal = limitar cantidadElementosCreados -- Asegura que la cantidad esté entre 10 y 25.

    return (map (crearEventoDesde idBase fechaBase) [1..cantidadFinal])

-- =========================
-- CREAR EVENTO
-- =========================

-- Nombre: crearEventoDesde
-- Descripción: Construye un evento individual con datos generados automáticamente.
-- Entradas: Int idBase, Day fechaBase, Int indice.
-- Salidas: Evento completo.
-- Validaciones: Usa lógica cíclica para generar datos consistentes y repetibles.
crearEventoDesde :: Int -> Day -> Int -> Evento

crearEventoDesde idBase fechaBase indice =

    let idGenerado = idBase + indice

        categoriaEvento = categorias !! (indice `mod` length categorias)

        (productoId, productoEvento) = productosConId !! (indice `mod` length productosConId)

        usuarioEvento = usuariosConId !! (indice `mod` length usuariosConId)

        metodoEvento = metodos !! (indice `mod` length metodos)

        estadoEvento = estados !! (indice `mod` length estados)

        valorEvento = fromIntegral (500 + ((idGenerado * 137 + indice * 97) `mod` 74500))-- Esto genera un valor entre 500 y 75000 de forma pseudoaleatoria basado en el ID y el índice.

        fechaEvento = generarFecha fechaBase indice

        cantidadEvento = 
            case categoriaEvento of
                Visualizacion -> 1
                Seguimiento -> 1
                Apartado -> 1 + (indice `mod` 3)-- Esto genera una cantidad entre 1 y 3 para Apartado.
                Compra -> 1 + (indice `mod` 8)-- Esto genera una cantidad entre 1 y 8 para Compra.
                Devolucion -> 1

    in Evento
        (idGenerado `mod` 9000000)-- Esto asegura que el ID no exceda 7 dígitos, aunque en la práctica no debería llegar a tanto.
        categoriaEvento
        valorEvento
        fechaEvento
        usuarioEvento
        productoId
        productoEvento
        cantidadEvento
        metodoEvento
        estadoEvento
        (calcularImpuesto categoriaEvento valorEvento)


-- Nombre: calcularImpuesto
-- Descripción: Calcula el impuesto aplicado a un evento.
-- Entradas: Categoria, Float valor.
-- Salidas: Float impuesto calculado.
-- Validaciones: Solo aplica 13% si la categoría es Compra.
calcularImpuesto :: Categoria -> Float -> Float

calcularImpuesto categoriaEvento valorEvento =

    if categoriaEvento == Compra-- Solo las compras tienen impuesto, el resto no.

        then redondear (valorEvento * 0.13)

        else 0

-- Nombre: redondear
-- Descripción: Redondea un número a 2 decimales.
-- Entradas: Float.
-- Salidas: Float redondeado.
-- Validaciones: No valida rango del número.
redondear :: Float -> Float

redondear n = fromIntegral (round (n * 100)) / 100


-- Nombre: obtenerUltimoId
-- Descripción: Obtiene el ID más alto de la lista de eventos.
-- Entradas: Lista de Evento.
-- Salidas: Int con el ID máximo.
-- Validaciones: Si la lista está vacía devuelve -1.
obtenerUltimoId :: [Evento] -> Int

obtenerUltimoId [] = -1-- Si no hay eventos, el próximo ID será 0, así que el último ID se considera -1.
obtenerUltimoId eventos = maximum (map idEvento eventos)