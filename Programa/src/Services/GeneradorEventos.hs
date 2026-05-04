module Services.GeneradorEventos where

import Data.Time
import Types.Modelos

--------------------------------------------------------------------------------
-- Nombre: categorias
--
-- Objetivo: lista fija de categorías del sistema
--
-- Entradas: ninguna
--
-- Salida: lista de valores Categoria
--
-- Restricciones:
--   - es constante durante toda la ejecución
--------------------------------------------------------------------------------
categorias :: [Categoria]
categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]

--------------------------------------------------------------------------------
-- Nombre: metodosPago
--
-- Objetivo: lista fija de métodos de pago disponibles
--
-- Entradas: ninguna
--
-- Salida: lista de MetodoPago
--------------------------------------------------------------------------------
metodosPago :: [MetodoPago]
metodosPago = [Tarjeta, Sinpe, Efectivo]

--------------------------------------------------------------------------------
-- Nombre: estadosEvento
--
-- Objetivo: lista fija de estados posibles de un evento
--
-- Entradas: ninguna
--
-- Salida: lista de Estado
--------------------------------------------------------------------------------
estadosEvento :: [Estado]
estadosEvento = [Pendiente, Completado, Cancelado]

--------------------------------------------------------------------------------
-- Nombre: productos
--
-- Objetivo: lista fija de productos simulados del sistema
--
-- Entradas: ninguna
--
-- Salida: lista de productos
--
-- Restricciones:
--   - usada únicamente para generación de datos
--------------------------------------------------------------------------------
productos :: [Producto]
productos =
    [ "Laptop", "Celular", "Tablet", "Audifonos", "Teclado", "Mouse"
    , "Monitor", "Impresora", "Camara", "Consola", "Smartwatch"
    , "Bocina", "Microfono", "Router", "DiscoDuro", "SSD"
    , "USB", "Cargador", "Proyector", "TV", "Silla Gamer"
    ]

--------------------------------------------------------------------------------
-- Nombre: limitarCantidad
--
-- Objetivo: restringe la cantidad de eventos generados
--
-- Entradas: número entero
--
-- Salida: número ajustado entre 10 y 25
--
-- Restricciones:
--   - mínimo 10
--   - máximo 25
--------------------------------------------------------------------------------
limitarCantidad :: Int -> Int
limitarCantidad n
    | n < 10 = 10
    | n > 25 = 25
    | otherwise = n

--------------------------------------------------------------------------------
-- Nombre: obtenerFechaActual
--
-- Objetivo: obtiene la fecha actual del sistema
--
-- Entradas: ninguna
--
-- Salida: día actual (IO Day)
--
-- Restricciones:
--   depende del sistema operativo
--------------------------------------------------------------------------------
obtenerFechaActual :: IO Day
obtenerFechaActual = do
    utctDay <$> getCurrentTime

--------------------------------------------------------------------------------
-- Nombre: obtenerMilisegundosActuales
--
-- Objetivo: obtiene un valor de tiempo en milisegundos para aleatoriedad
--
-- Entradas: ninguna
--
-- Salida: entero con milisegundos del día
--------------------------------------------------------------------------------
obtenerMilisegundosActuales :: IO Int
obtenerMilisegundosActuales = do
    tiempo <- getCurrentTime

    let segundosDelDia = utctDayTime tiempo
        milisegundos = floor (segundosDelDia * 1000)

    return milisegundos

--------------------------------------------------------------------------------
-- Nombre: obtenerFechaEvento
--
-- Objetivo: genera una fecha simulada para un evento
--
-- Entradas:
--   - fechaBase: fecha inicial
--   - indice: identificador del evento
--
-- Salida: fecha en formato numérico YYYYMMDD
--
-- Restricciones:
--   generación pseudoaleatoria controlada
--------------------------------------------------------------------------------
obtenerFechaEvento :: Day -> Int -> IO Int
obtenerFechaEvento fechaBase indice = do
    milisegundos <- obtenerMilisegundosActuales

    let ruido = milisegundos `mod` 997

        desplazamiento = (indice * 41 + indice * indice + ruido * 17) `mod` 730

        fechaDesplazada = addDays (fromIntegral desplazamiento) fechaBase

        (anio, mes, dia) = toGregorian fechaDesplazada

        fechaFormateada = fromIntegral anio * 10000 + mes * 100 + dia

    return fechaFormateada

--------------------------------------------------------------------------------
-- Nombre: productosConCodigo
--
-- Objetivo: asigna códigos internos a productos
--
-- Entradas: ninguna
--
-- Salida: lista (codigo, producto)
--------------------------------------------------------------------------------
productosConCodigo :: [(String, Producto)]
productosConCodigo =
    let generarCodigo i = "P" ++ show (200 + i)
        codigos = map generarCodigo [0..]
    in zip codigos productos

--------------------------------------------------------------------------------
-- Nombre: usuariosConCodigo
--
-- Objetivo: lista de usuarios simulados del sistema
--
-- Entradas: ninguna
--
-- Salida: lista de identificadores de usuario
--------------------------------------------------------------------------------
usuariosConCodigo :: [String]
usuariosConCodigo =
    let generarCodigo i = "U" ++ show (300 + i)
    in map generarCodigo [0..299]


--------------------------------------------------------------------------------
-- Nombre: obtenerProducto
--
-- Objetivo: obtiene un producto de forma cíclica
--
-- Entradas: índice
--
-- Salida: (código, producto)
--------------------------------------------------------------------------------
obtenerProducto :: Int -> (String, Producto)
obtenerProducto indice =
    let limite = length productosConCodigo
        posicion = indice `mod` limite
    in productosConCodigo !! posicion

--------------------------------------------------------------------------------
-- Nombre: obtenerUsuario
--
-- Objetivo: obtiene usuario de forma cíclica
--
-- Entradas: índice
--
-- Salida: usuario
--------------------------------------------------------------------------------
obtenerUsuario :: Int -> String
obtenerUsuario indice =
    let limite = length usuariosConCodigo
        posicion = indice `mod` limite
    in usuariosConCodigo !! posicion

--------------------------------------------------------------------------------
-- Nombre: generarEventos
--
-- Objetivo: genera automáticamente nuevos eventos basados en el tamaño actual
--           del sistema
--
-- Entradas:
--   eventosExistentes: lista de eventos ya registrados
--
-- Salida:
--   lista de eventos nuevos generados (IO [Evento])
--
-- Restricciones:
--   - la cantidad generada depende del tamaño actual del sistema
--   - usa valores simulados para crear eventos
--------------------------------------------------------------------------------
generarEventos :: [Evento] -> IO [Evento]
generarEventos eventosExistentes = do
    fechaBase <- obtenerFechaActual

    let
        cantidadBase = length eventosExistentes

        cantidadEventos = limitarCantidad (10 + (cantidadBase `div` 5) `mod` 16)-- Genera entre 10 y 25 eventos dependiendo del tamaño actual

        indices = [cantidadBase + 1 .. cantidadBase + cantidadEventos]

    mapM (crearEvento fechaBase) indices

--------------------------------------------------------------------------------
-- Nombre: crearEvento
--
-- Objetivo: construye un evento completo con datos simulados
--
-- Entradas:
--   fechaBase: fecha inicial para generar eventos
--   indice: identificador secuencial del evento
--
-- Salida:
--   evento completo (IO Evento)
--
-- Restricciones:
--   - todos los valores se generan de forma simulada
--------------------------------------------------------------------------------
crearEvento :: Day -> Int -> IO Evento
crearEvento fechaBase indice = do

    fecha <- obtenerFechaEvento fechaBase indice

    milisegundos <- obtenerMilisegundosActuales

    let
        idEvento = indice - 1

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

--------------------------------------------------------------------------------
-- Nombre: determinarCategoria
--
-- Objetivo: asigna una categoría simulada usando un cálculo pseudoaleatorio
--
-- Entradas:
--   indice: posición del evento
--   ruido: valor aleatorio basado en tiempo
--   idEvento: identificador del evento
--
-- Salida:
--   categoría del evento
--
-- Restricciones:
--   distribución aproximada por porcentajes
--------------------------------------------------------------------------------
determinarCategoria :: Int -> Int -> Int -> Categoria
determinarCategoria indice ruido idEvento =
    let
        mezcla = indice * 131 + ruido * 97 + idEvento * 53 + (indice `div` 3) * 17

        idx = abs mezcla `mod` 100

    in
        if idx < 20 then Visualizacion --20%
        else if idx < 40 then Apartado--20%
        else if idx < 70 then Compra--30%
        else if idx < 85 then Seguimiento--15%
        else Devolucion--15%

--------------------------------------------------------------------------------
-- Nombre: seleccionarMetodo
--
-- Objetivo: asigna un método de pago de forma cíclica
--
-- Entradas: índice
-- Salida: método de pago
--------------------------------------------------------------------------------
seleccionarMetodo :: Int -> MetodoPago
seleccionarMetodo indice =
    let limite = length metodosPago
        posicion = indice `mod` limite
    in metodosPago !! posicion

--------------------------------------------------------------------------------
-- Nombre: seleccionarEstado
--
-- Objetivo: asigna un estado de forma cíclica
--
-- Entradas: índice
-- Salida: estado del evento
--------------------------------------------------------------------------------
seleccionarEstado :: Int -> Estado
seleccionarEstado indice =
    let limite = length estadosEvento
        posicion = indice `mod` limite
    in estadosEvento !! posicion

--------------------------------------------------------------------------------
-- Nombre: calcularValor
--
-- Objetivo: genera un valor monetario simulado para el evento
--
-- Entradas:
--   idEvento, indice, ruido
--
-- Salida: valor del evento (Float)
--------------------------------------------------------------------------------
calcularValor :: Int -> Int -> Int -> Float
calcularValor idEvento indice ruido =
    let mezcla =  idEvento * 131 +  indice * 97 +  ruido * 19

        rango =  mezcla `mod` 74500

        valorFinal =  max 500 (500 + rango)

    in fromIntegral valorFinal

--------------------------------------------------------------------------------
-- Nombre: calcularCantidad
--
-- Objetivo: determina la cantidad asociada al evento según su categoría
--
-- Entradas:
--   categoria, indice
--
-- Salida: cantidad del evento
--------------------------------------------------------------------------------
calcularCantidad :: Categoria -> Int -> Int
calcularCantidad categoria indice =
    case categoria of
        Visualizacion -> 1
        Seguimiento   -> 1
        Apartado      -> 1
        Compra        -> 1 + (indice `mod` 3)-- Compra puede tener entre 1 y 3 unidades
        Devolucion    -> 1

--------------------------------------------------------------------------------
-- Nombre: obtenerUltimoId
--
-- Objetivo: obtiene el último id registrado en la lista de eventos
--
-- Entradas: lista de eventos
--
-- Salida: último id o -1 si no hay eventos
--------------------------------------------------------------------------------
obtenerUltimoId :: [Evento] -> Int
obtenerUltimoId eventos =
        if null eventos
        then -1 -- Si no hay eventos, el próximo id será 0, por lo que el último id es -1
        else maximum (map idEvento eventos)