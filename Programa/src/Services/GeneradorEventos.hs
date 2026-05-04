module Services.GeneradorEventos where

import Data.Time
import Types.Modelos

--------------------------------------------------------------------------------
-- Nombre: listaCategorias
-- Entrada: ninguna, es una lista fija de categorías del sistema
-- Salida: lista de tipos de acciones que puede tener un evento
-- Restricciones:
--   No cambia durante la ejecución del programa
--------------------------------------------------------------------------------
categorias :: [Categoria]
categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]

--------------------------------------------------------------------------------
-- Nombre: listaMetodosPago
-- Entrada: ninguna, es una lista fija de métodos de pago disponibles
-- Salida: lista de formas de pago que puede usar un evento
-- Restricciones:
--   Valores definidos de forma estática
--------------------------------------------------------------------------------
metodosPago :: [MetodoPago]
metodosPago = [Tarjeta, Sinpe, Efectivo]

--------------------------------------------------------------------------------
-- Nombre: listaEstadosEvento
-- Entrada: ninguna, es una lista fija de estados posibles de un evento
-- Salida: lista de estados que puede tener un evento
-- Restricciones:
--   No se modifica durante la ejecución
--------------------------------------------------------------------------------
estadosEvento :: [Estado]
estadosEvento = [Pendiente, Completado, Cancelado]

--------------------------------------------------------------------------------
-- Nombre: listaProductos
-- Entrada: ninguna, lista fija de productos del sistema
-- Salida: lista de productos disponibles para asignar a eventos
-- Restricciones:
--   Lista estática utilizada para generar datos simulados
--------------------------------------------------------------------------------
productos :: [Producto]
productos =
    [ "Laptop", "Celular", "Tablet", "Audifonos", "Teclado", "Mouse"
    , "Monitor", "Impresora", "Camara", "Consola", "Smartwatch"
    , "Bocina", "Microfono", "Router", "DiscoDuro", "SSD"
    , "USB", "Cargador", "Proyector", "TV", "Silla Gamer"
    ]

--------------------------------------------------------------------------------
-- Nombre: limitarCantidadEventos
-- Entrada: número entero que representa cantidad de eventos
-- Salida: ajusta la cantidad para que esté entre un mínimo y un máximo
-- Restricciones:
--   Si el número es menor a 10 se ajusta a 10
--   Si es mayor a 25 se ajusta a 25
--------------------------------------------------------------------------------
limitarCantidad :: Int -> Int
limitarCantidad n
    | n < 10 = 10
    | n > 25 = 25
    | otherwise = n

--------------------------------------------------------------------------------
-- Nombre: obtenerFechaActualSistema
-- Entrada: ninguna
-- Salida: fecha actual del sistema en formato de día
-- Restricciones:
--   Depende del reloj del sistema operativo
--------------------------------------------------------------------------------
obtenerFechaActual :: IO Day
obtenerFechaActual = utctDay <$> getCurrentTime

--------------------------------------------------------------------------------
-- Nombre: obtenerMilisegundosSistema
-- Entrada: ninguna
-- Salida: número entero basado en milisegundos actuales
-- Restricciones:
--   Se usa como valor de ruido para generar variaciones
--------------------------------------------------------------------------------
obtenerMilisegundosActuales :: IO Int
obtenerMilisegundosActuales = do
    tiempo <- getCurrentTime
    return (floor (utctDayTime tiempo * 1000) :: Int)


--------------------------------------------------------------------------------
-- Nombre: generarFechaEvento
-- Entrada:
--   fechaBase: fecha inicial desde donde se generan eventos
--   indice: número del evento en la secuencia
-- Salida: fecha generada para el evento en formato numérico YYYYMMDD
-- Restricciones:
--   La fecha generada depende de cálculos aleatorios controlados
--------------------------------------------------------------------------------
obtenerFechaEvento :: Day -> Int -> IO Int
obtenerFechaEvento fechaBase indice = do
    milisegundos <- obtenerMilisegundosActuales

    let ruido = milisegundos `mod` 997
        desplazamiento = (indice * 41 + indice * indice + ruido * 17) `mod` 730

        nuevaFecha = addDays (fromIntegral desplazamiento) fechaBase

        (anio, mes, dia) = toGregorian nuevaFecha

    return (fromIntegral anio * 10000 + mes * 100 + dia)


--------------------------------------------------------------------------------
-- Nombre: productosConCodigoInterno
-- Entrada: ninguna, lista derivada de productos con códigos internos
-- Salida: pares de código y producto
-- Restricciones:
--   Es una estructura fija generada al iniciar el programa
--------------------------------------------------------------------------------
productosConCodigo :: [(String, Producto)]
productosConCodigo = zip (map (\i -> "P" ++ show (200 + i)) [0..]) productos

--------------------------------------------------------------------------------
-- Nombre: usuariosConCodigoInterno
-- Entrada: ninguna, lista de usuarios simulados del sistema
-- Salida: lista de identificadores de usuario
-- Restricciones:
--   Máximo 300 usuarios generados
--------------------------------------------------------------------------------
usuariosConCodigo :: [String]
usuariosConCodigo = map (\i -> "U" ++ show (300 + i)) [0..299]

--------------------------------------------------------------------------------
-- Nombre: obtenerProductoPorIndice
-- Entrada:
--   indice: número entero usado para seleccionar un producto
-- Salida: producto asociado con un código
-- Restricciones:
--   Usa módulo para evitar desbordamiento de lista
--------------------------------------------------------------------------------
obtenerProducto :: Int -> (String, Producto)
obtenerProducto indice = productosConCodigo !! (indice `mod` length productosConCodigo)

--------------------------------------------------------------------------------
-- Nombre: obtenerUsuarioPorIndice
-- Entrada:
--   indice: número entero usado para seleccionar un usuario
-- Salida: identificador de usuario
-- Restricciones:
--   Selección cíclica de usuarios disponibles
--------------------------------------------------------------------------------
obtenerUsuario :: Int -> String
obtenerUsuario indice = usuariosConCodigo !! (indice `mod` length usuariosConCodigo)

--------------------------------------------------------------------------------
-- Nombre: generarEventos
-- Entrada:
--   eventosExistentes: lista de eventos ya registrados en el sistema
-- Salida: lista de nuevos eventos generados automáticamente
-- Restricciones:
--   La cantidad de eventos generados depende del tamaño actual del sistema
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
-- Nombre: crearEventoNuevo
-- Entrada:
--   fechaBase: fecha desde la que se generan eventos
--   indice: identificador del evento
-- Salida: evento completo con todos sus datos generados
-- Restricciones:
--   El evento se genera con datos simulados basados en cálculos internos
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
-- Nombre: determinarCategoriaEvento
-- Entrada: índice, ruido y id del evento
-- Salida: categoría asignada al evento
-- Restricciones:
--   Se basa en cálculo pseudoaleatorio controlado
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
-- Nombre: seleccionarMetodoPago
-- Entrada: índice numérico
-- Salida: método de pago asignado
-- Restricciones:
--   Selección cíclica
--------------------------------------------------------------------------------
seleccionarMetodo :: Int -> MetodoPago
seleccionarMetodo indice = metodosPago !! (indice `mod` length metodosPago)

--------------------------------------------------------------------------------
-- Nombre: seleccionarEstadoEvento
-- Entrada: índice numérico
-- Salida: estado del evento
-- Restricciones:
--   Selección cíclica
--------------------------------------------------------------------------------
seleccionarEstado :: Int -> Estado
seleccionarEstado indice = estadosEvento !! (indice `mod` length estadosEvento)

--------------------------------------------------------------------------------
-- Nombre: calcularValorEvento
-- Entrada: id, índice y ruido
-- Salida: valor monetario del evento
-- Restricciones:
--   Generación simulada de valores
--------------------------------------------------------------------------------
calcularValor :: Int -> Int -> Int -> Float
calcularValor idEvento indice ruido =
    let 
        mezcla = idEvento * 131 + indice * 97 + ruido * 19
        rango = mezcla `mod` 74500
    in 
        fromIntegral (max 500 (500 + rango))

--------------------------------------------------------------------------------
-- Nombre: calcularCantidadEvento
-- Entrada: categoría e índice
-- Salida: cantidad asociada al evento
-- Restricciones:
--   Depende del tipo de evento
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
-- Nombre: obtenerUltimoIdEvento
-- Entrada: lista de eventos
-- Salida: último id registrado
-- Restricciones:
--   Retorna -1 si no hay eventos
--------------------------------------------------------------------------------
obtenerUltimoId :: [Evento] -> Int
obtenerUltimoId eventos =
        if null eventos
        then -1 -- Si no hay eventos, el próximo id será 0, por lo que el último id es -1
        else maximum (map idEvento eventos)