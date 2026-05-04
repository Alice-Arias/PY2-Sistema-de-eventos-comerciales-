module Utils.Calculos where

import Types.Modelos
import Data.Time (Day, addDays)
import Data.List (sortOn, nub, maximumBy, minimumBy, groupBy, sortBy, sort)
import Data.Ord (comparing)
import Data.Function (on)
import Types.Fecha (intToDay, formatearFecha, extraerAnio, extraerMes)
import Text.Read (readMaybe)
import Utils.Formato
import Utils.Colores

--------------------------------------------------------------------------------
-- Nombre: calcularSubtotal
-- Entrada:
--   precioUnitario: precio de una unidad del producto
--   cantidad: número de unidades del evento
-- Salida:
--   subtotal del evento
-- Restricciones:
--   - Multiplica precio por cantidad
--------------------------------------------------------------------------------
calcularSubtotal :: Float -> Int -> Float
calcularSubtotal precioUnitario cantidad = precioUnitario * fromIntegral cantidad

--------------------------------------------------------------------------------
-- Nombre: calcularImpuesto13
-- Entrada:
--   subtotal: monto base del evento
-- Salida:
--   impuesto del 13% del subtotal
-- Restricciones:
--   - Solo cálculo matemático
--------------------------------------------------------------------------------
calcularImpuesto13 :: Float -> Float
calcularImpuesto13 subtotal = subtotal * 0.13

--------------------------------------------------------------------------------
-- Nombre: redondear
-- Entrada:
--   valor: número decimal
-- Salida:
--   valor redondeado a 2 decimales
-- Restricciones:
--   - Solo precisión numérica
--------------------------------------------------------------------------------
redondear :: Float -> Float
redondear valor = fromIntegral (round (valor * 100)) / 100

--------------------------------------------------------------------------------
-- Nombre: calcularTotales
-- Entrada:
--   evento: registro del sistema
-- Salida:
--   evento con impuesto y total actualizado
-- Restricciones:
--   - Depende de la categoría del evento
--------------------------------------------------------------------------------
calcularTotales :: Evento -> Evento
calcularTotales evento =

    let 
        precioUnitario = valor evento

        cantidadUnidades = cantidad evento

        subtotal = redondear (calcularSubtotal precioUnitario cantidadUnidades)

    in case categoria evento of

        Compra ->

            let imp = redondear (calcularImpuesto13 subtotal)
            in evento { impuesto = imp, total = subtotal + imp }

        _ ->
            evento { impuesto = 0, total = subtotal }

--------------------------------------------------------------------------------
-- Nombre: actualizarTotales
-- Entrada:
--   listaEventos: lista de eventos
-- Salida:
--   lista de eventos con totales actualizados
-- Restricciones:
--   - Aplica cálculo a todos los elementos
--------------------------------------------------------------------------------
actualizarTotales :: [Evento] -> [Evento]
actualizarTotales = map calcularTotales

--------------------------------------------------------------------------------
-- Nombre: porcentaje
-- Entrada:
--   parte: valor parcial
--   total: valor total
-- Salida:
--   porcentaje calculado
-- Restricciones:
--   - Evitar división entre cero fuera de esta función
--------------------------------------------------------------------------------
porcentaje :: Float -> Float -> Float
porcentaje parte total = (parte / total) * 100

--------------------------------------------------------------------------------
-- Nombre: totalAjustado
-- Entrada:
--   evento: registro individual
-- Salida:
--   total considerando devoluciones
-- Restricciones:
--   - Si es devolución, se invierte el valor
--------------------------------------------------------------------------------
totalAjustado :: Evento -> Float
totalAjustado e =
    case categoria e of

        Devolucion -> -(total e)
        _          -> total e

--------------------------------------------------------------------------------
-- Nombre: calcularSumaTotales
-- Entrada:
--   listaEventos: lista de eventos
-- Salida:
--   suma total ajustada
-- Restricciones:
--   - Usa total ajustado
--------------------------------------------------------------------------------
calcularSumaTotales :: [Evento] -> Float

calcularSumaTotales = sum . map totalAjustado

--------------------------------------------------------------------------------
-- Nombre: contarEventos
-- Entrada:
--   listaEventos: lista de eventos
-- Salida:
--   cantidad total de eventos
-- Restricciones:
--   - Ninguna
--------------------------------------------------------------------------------
contarEventos :: [Evento] -> Int
contarEventos = length

--------------------------------------------------------------------------------
-- Nombre: calcularPromedioLocal
-- Entrada:
--   suma: valor total
--   cantidad: número de elementos
-- Salida:
--   promedio calculado
-- Restricciones:
--   - Evita división entre cero
--------------------------------------------------------------------------------
calcularPromedioLocal :: Float -> Int -> Float
calcularPromedioLocal _ 0 = 0
calcularPromedioLocal suma cantidad = suma / fromIntegral cantidad

--------------------------------------------------------------------------------
-- Nombre: ordenarEventos
-- Entrada:
--   listaEventos: eventos sin ordenar
-- Salida:
--   eventos ordenados por fecha
-- Restricciones:
--   - Orden ascendente
--------------------------------------------------------------------------------
ordenarEventos :: [Evento] -> [Evento]
ordenarEventos = sortOn timestamp

--------------------------------------------------------------------------------
-- Nombre: obtenerFechaInicio
-- Entrada:
--   listaEventos: lista de eventos
-- Salida:
--   fecha más antigua
-- Restricciones:
--   - Lista no vacía
--------------------------------------------------------------------------------
obtenerFechaInicio :: [Evento] -> Day
obtenerFechaInicio eventos = intToDay (timestamp (head eventos))

--------------------------------------------------------------------------------
-- Nombre: obtenerFechaFin
-- Entrada:
--   listaEventos: lista de eventos
-- Salida:
--   fecha más reciente
-- Restricciones:
--   - Lista no vacía
--------------------------------------------------------------------------------
obtenerFechaFin :: [Evento] -> Day
obtenerFechaFin eventos = intToDay (timestamp (last eventos))

--------------------------------------------------------------------------------
-- Nombre: sumarDias
-- Entrada:
--   fecha: fecha base
--   dias: cantidad de días
-- Salida:
--   nueva fecha
-- Restricciones:
--   - Usa addDays
--------------------------------------------------------------------------------
sumarDias :: Day -> Int -> Day
sumarDias fecha dias = addDays (fromIntegral dias) fecha

--------------------------------------------------------------------------------
-- Nombre: separarEventos
-- Entrada:
--   fechaLimite: fecha de corte
--   listaEventos: lista de eventos
-- Salida:
--   dos listas separadas
-- Restricciones:
--   - Lista ordenada previamente
--------------------------------------------------------------------------------
separarEventos :: Day -> [Evento] -> ([Evento], [Evento])
separarEventos fechaLimite = span (\e -> intToDay (timestamp e) < fechaLimite)

--------------------------------------------------------------------------------
-- Nombre: ajustarFinIntervalo
-- Entrada:
--   fechaLimite: fecha base
-- Salida:
--   fecha ajustada
-- Restricciones:
--   - Resta un día
--------------------------------------------------------------------------------
ajustarFinIntervalo :: Day -> Day -> Day
ajustarFinIntervalo fechaLimite =  min (addDays (-1) fechaLimite)

--------------------------------------------------------------------------------
-- Nombre: obtenerPromedioCategoria
-- Entrada:
--   promedios: lista (categoria, promedio)
--   cat: categoría buscada
-- Salida:
--   promedio de la categoría o Nothing
-- Restricciones:
--   - Usa lookup
--------------------------------------------------------------------------------
obtenerPromedioCategoria :: [(Categoria, Float)] -> Categoria -> Maybe Float
obtenerPromedioCategoria promedios cat = lookup cat promedios

--------------------------------------------------------------------------------
-- Nombre: obtenerCategoriasUnicas
-- Entrada:
--   listaEventos: lista de eventos
-- Salida:
--   categorías sin repetir
-- Restricciones:
--   - Usa nub
--------------------------------------------------------------------------------
obtenerCategoriasUnicas :: [Evento] -> [Categoria]
obtenerCategoriasUnicas listaEventos = nub (map categoria listaEventos)

--------------------------------------------------------------------------------
-- Nombre: filtrarEventosPorCategoria
-- Entrada:
--   listaEventos: eventos
--   categoriaBuscada: categoría
-- Salida:
--   eventos filtrados
-- Restricciones:
--   - Comparación exacta
--------------------------------------------------------------------------------
filtrarEventosPorCategoria :: [Evento] -> Categoria -> [Evento]
filtrarEventosPorCategoria listaEventos categoriaBuscada = filter (\evento -> categoria evento == categoriaBuscada) listaEventos

--------------------------------------------------------------------------------
-- Nombre: sumarValores
-- Entrada:
--   listaEventos: lista de eventos
-- Salida:
--   suma de valores
-- Restricciones:
--   - Usa valor bruto
--------------------------------------------------------------------------------
sumarValores :: [Evento] -> Float
sumarValores = sum . map valor

--------------------------------------------------------------------------------
-- Nombre: calcularPromedio
-- Entrada:
--   suma: valor total
--   cantidad: número de elementos
-- Salida:
--   promedio
-- Restricciones:
--   - Evita división por cero
--------------------------------------------------------------------------------
calcularPromedio :: Float -> Int -> Float
calcularPromedio _ 0 = 0
calcularPromedio suma cantidad = suma / fromIntegral cantidad

--------------------------------------------------------------------------------
-- EVENTOS EXTREMOS
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Nombre: eventoMaxCSV
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   evento con mayor total en formato CSV (id,total)
-- Restricciones:
--   - Usa total ajustado (incluye devoluciones negativas)
--------------------------------------------------------------------------------
eventoMaxCSV :: [Evento] -> String
eventoMaxCSV eventos =
    let e = maximumBy (compare `on` totalAjustado) eventos
    in show (idEvento e) ++ "," ++ limpiarFloat (totalAjustado e)


--------------------------------------------------------------------------------
-- Nombre: eventoMinCSV
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   evento mínimo sin devoluciones en formato CSV
-- Restricciones:
--   - Aplica función eventoMinSinDevolucionesCSV
--------------------------------------------------------------------------------
eventoMinCSV :: [Evento] -> String
eventoMinCSV = eventoMinSinDevolucionesCSV


--------------------------------------------------------------------------------
-- Nombre: totalReal
-- Entrada:
--   e: evento individual
-- Salida:
--   total calculado considerando impuesto y categoría
-- Restricciones:
--   - Recalcula subtotal e impuesto si es necesario
--------------------------------------------------------------------------------
totalReal :: Evento -> Float
totalReal e =
    let subtotal = valor e * fromIntegral (cantidad e)

        impuestoCalc =
            if categoria e == Compra
            then subtotal * 0.13
            else 0

        totalCalculado = subtotal + impuestoCalc

        totalBase =
            if total e == 0
            then totalCalculado
            else total e

    in case categoria e of
        Devolucion -> -totalBase
        _          -> totalBase


--------------------------------------------------------------------------------
-- Nombre: limpiarFloat
-- Entrada:
--   valor: número decimal
-- Salida:
--   número convertido a texto
-- Restricciones:
--   - Conversión simple sin formateo adicional
--------------------------------------------------------------------------------
limpiarFloat :: Float -> String
limpiarFloat = show


--------------------------------------------------------------------------------
-- Nombre: eventoMinSinDevolucionesCSV
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   evento mínimo en formato CSV excluyendo devoluciones
-- Restricciones:
--   - Ignora eventos de tipo Devolucion
--------------------------------------------------------------------------------
eventoMinSinDevolucionesCSV :: [Evento] -> String
eventoMinSinDevolucionesCSV eventos =
    let filtrados = filter (\e -> categoria e /= Devolucion) eventos
    in if null filtrados
        then "0,0"
        else
            let e = minimumBy (compare `on` totalAjustado) filtrados
            in show (idEvento e) ++ "," ++ limpiarFloat (totalAjustado e)


--------------------------------------------------------------------------------
-- Nombre: calcularDiaMasActivo
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   día con mayor cantidad de eventos (formato texto)
-- Restricciones:
--   - Agrupa eventos por fecha
--------------------------------------------------------------------------------
calcularDiaMasActivo :: [Evento] -> String
calcularDiaMasActivo eventos =   fst (maximumBy (compare `on` snd) (contarDias eventos))


--------------------------------------------------------------------------------
-- Nombre: contarDias
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   lista de tuplas (fecha, cantidad de eventos)
-- Restricciones:
--   - Agrupa eventos por fecha formateada
--------------------------------------------------------------------------------
contarDias :: [Evento] -> [(String, Int)]
contarDias =
    map (\xs -> (formatearFecha (timestamp (head xs)), length xs))
    . groupBy ((==) `on` (formatearFecha . timestamp))
    . sortBy (compare `on` (formatearFecha . timestamp))


--------------------------------------------------------------------------------
-- Nombre: quitarGuiones
-- Entrada:
--   texto: cadena de caracteres
-- Salida:
--   texto sin guiones
-- Restricciones:
--   - Elimina únicamente '-'
--------------------------------------------------------------------------------
quitarGuiones :: String -> String
quitarGuiones = filter (/= '-')


--------------------------------------------------------------------------------
-- Nombre: formatearTotal
-- Entrada:
--   evento: registro del sistema
--   subtotal: valor calculado base
-- Salida:
--   texto formateado con color según estado
-- Restricciones:
--   - Depende del total del evento
--------------------------------------------------------------------------------
formatearTotal :: Evento -> Float -> String
formatearTotal evento subtotal =
    if total evento == 0
        then errorMsg (formatearMonto subtotal)
        else okMsg (formatearMonto (total evento))


--------------------------------------------------------------------------------
-- Nombre: calcularMontoTotal
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   suma total ajustada de todos los eventos
-- Restricciones:
--   - Usa totalAjustado
--------------------------------------------------------------------------------
calcularMontoTotal :: [Evento] -> Float
calcularMontoTotal = sum . map totalAjustado


--------------------------------------------------------------------------------
-- Nombre: obtenerAnios
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   lista de años únicos ordenados
-- Restricciones:
--   - Sin duplicados
--------------------------------------------------------------------------------
obtenerAnios :: [Evento] -> [Integer]
obtenerAnios eventos = sort (nub (map (extraerAnio . timestamp) eventos))


--------------------------------------------------------------------------------
-- Nombre: obtenerCategorias
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   lista de categorías únicas
-- Restricciones:
--   - Sin duplicados
--------------------------------------------------------------------------------
obtenerCategorias :: [Evento] -> [Categoria]
obtenerCategorias eventos =  nub (map categoria eventos)


--------------------------------------------------------------------------------
-- Nombre: promedioCategoriaAnioCalc
-- Entrada:
--   eventos: lista de eventos
--   anio: año a filtrar
--   cat: categoría a filtrar
-- Salida:
--   promedio de valores filtrados
-- Restricciones:
--   - Si no hay datos devuelve 0
--------------------------------------------------------------------------------
promedioCategoriaAnioCalc :: [Evento] -> Integer -> Categoria -> Float
promedioCategoriaAnioCalc eventos anio cat =
    let filtrados = filter (\e ->  categoria e == cat &&   extraerAnio (timestamp e) == anio  ) eventos

        suma = sum (map totalAjustado filtrados)
        cantidad = length filtrados

    in if cantidad == 0
        then 0
        else suma / fromIntegral cantidad


--------------------------------------------------------------------------------
-- Nombre: eMinimoSinDev
-- Entrada:
--   eventos: lista de eventos del sistema
-- Salida:
--   evento mínimo válido sin devoluciones
-- Restricciones:
--   - Filtra eventos válidos
--------------------------------------------------------------------------------
eMinimoSinDev :: [Evento] -> String
eMinimoSinDev eventos =
    let filtrados = filter esValido eventos
    in if null filtrados
        then "Sin datos"
        else
            let e = minimumBy (compare `on` totalSinDevoluciones) filtrados
            in show (idEvento e) ++ "," ++ limpiarFloat (totalSinDevoluciones e)


--------------------------------------------------------------------------------
-- Nombre: esValido
-- Entrada:
--   e: evento individual
-- Salida:
--   True si el evento es válido
-- Restricciones:
--   - No permite devoluciones ni valores negativos
--------------------------------------------------------------------------------
esValido :: Evento -> Bool
esValido e =
    categoria e /= Devolucion &&
    valor e > 0 &&
    cantidad e > 0


--------------------------------------------------------------------------------
-- Nombre: totalSinDevoluciones
-- Entrada:
--   e: evento individual
-- Salida:
--   total calculado sin devoluciones
-- Restricciones:
--   - Aplica impuesto solo a compras
--------------------------------------------------------------------------------
totalSinDevoluciones :: Evento -> Float
totalSinDevoluciones e =
    let subtotal = valor e * fromIntegral (cantidad e)
    in case categoria e of
        Compra -> subtotal * 1.13
        _      -> subtotal