module Services.BusquedaRangFechas where

import Types.Evento
import Utils.Colores
import Utils.Formato (formatearMonto)
import Utils.FechaSegura (parseFecha)
import Data.List (sortBy)

buscarPorRangoFechas :: [Evento] -> String -> String -> IO ()
buscarPorRangoFechas eventos fechaInicioStr fechaFinStr =
    case (parseFecha fechaInicioStr, parseFecha fechaFinStr) of

        (Just fechaInicio, Just fechaFin) -> procesarBusqueda eventos fechaInicio fechaFin fechaInicioStr fechaFinStr

        _ ->
            putStrLn (errorMsg "Fechas inválidas. Usa formato dd-mm-yyyy")


procesarBusqueda :: [Evento] -> Int -> Int -> String -> String -> IO ()
procesarBusqueda eventos fechaInicio fechaFin fechaInicioStr fechaFinStr = do

    let (inicio, fin) = ordenarFechas fechaInicio fechaFin

        eventosFiltrados = filtrarPorRango eventos inicio fin

        eventosOrdenados = ordenarPorFecha eventosFiltrados

    imprimirEncabezado fechaInicioStr fechaFinStr

    imprimirTabla

    if null eventosOrdenados
    then putStrLn (errorMsg "No hay eventos en este rango.")
    else mapM_ imprimirFila eventosOrdenados

    imprimirResumen eventosOrdenados


-- =========================
-- FILTRO Y ORDEN
-- =========================

filtrarPorRango :: [Evento] -> Int -> Int -> [Evento]
filtrarPorRango eventos inicio fin = filter (\e -> timestamp e >= inicio && timestamp e <= fin) eventos


ordenarPorFecha :: [Evento] -> [Evento]
ordenarPorFecha = sortBy (\a b -> compare (timestamp a) (timestamp b))


ordenarFechas :: Int -> Int -> (Int, Int)
ordenarFechas a b
    | a <= b    = (a, b)
    | otherwise = (b, a)


imprimirEncabezado :: String -> String -> IO ()
imprimirEncabezado inicio fin = do
    putStrLn (titulo "╔══════════════════════════════════════╗")
    putStrLn (titulo "   BÚSQUEDA POR RANGO DE FECHAS       ")
    putStrLn (titulo "╚══════════════════════════════════════╝")

    putStrLn (infoMsg (" Desde: " ++ inicio))
    putStrLn (infoMsg (" Hasta: " ++ fin))



imprimirTabla :: IO ()
imprimirTabla = do
    putStrLn (separador "════════════════════════════════════════════════════════════════════")

    putStrLn (
        okMsg     (col "FECHA" 12) ++ "|" ++
        okMsg     (col "ID" 8) ++ "|" ++
        titulo    (col "CATEGORIA" 14) ++ "|" ++
        texto     (col "VALOR" 14) ++ "|" ++
        warningMsg (col "CANT" 6) ++ "|" ++
        errorMsg  (col "TOTAL" 14))

    putStrLn (separador "════════════════════════════════════════════════════════════════════")


imprimirFila :: Evento -> IO ()
imprimirFila evento = do

    let subtotal = calcularSubtotal evento
        totalTxt = calcularTotalTexto evento subtotal

    putStrLn (
        texto   (col (formatearFecha (timestamp evento)) 12) ++ "|" ++
        infoMsg (col (show (idEvento evento)) 8) ++ "|" ++
        titulo  (col (show (categoria evento)) 14) ++ "|" ++
        texto   (col (formatearMonto (valor evento)) 14) ++ "|" ++
        warningMsg (col (show (cantidad evento)) 6) ++ "|" ++
        totalTxt)
    putStrLn (separador "--------------------------------------------------------------------")



imprimirResumen :: [Evento] -> IO ()
imprimirResumen eventos = do
    putStrLn (separador "════════════════════════════════════════════════════════════════════")

    putStrLn (okMsg (" Total encontrados: " ++ show (length eventos)))
    putStrLn (titulo (" Monto total: " ++ formatearMonto (sum (map total eventos))))

    putStrLn (separador "════════════════════════════════════════════════════════════════════")



calcularSubtotal :: Evento -> Float
calcularSubtotal e =
    valor e * fromIntegral (cantidad e)


calcularTotalTexto :: Evento -> Float -> String
calcularTotalTexto e subtotal =
    if total e == 0
        then errorMsg (formatearMonto subtotal)
        else okMsg (formatearMonto (total e))


formatearFecha :: Int -> String
formatearFecha fecha =
    let s = show fecha
    in 
        if length s == 8
        then 
            take 2 (drop 6 s) ++ "-" ++
            take 2 (drop 4 s) ++ "-" ++
            take 4 s
        else s


col :: String -> Int -> String
col txt n = take n (txt ++ repeat ' ')