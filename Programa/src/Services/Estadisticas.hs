module Services.Estadisticas where

import Types.Evento
import Types.Estadisticas
import Data.List
import Data.Function (on)
import Utils.Colores
import Utils.Formato (formatearMonto)
import Text.Read (readMaybe)

generarEstadisticas :: [Evento] -> IO Estadistica
generarEstadisticas eventos = do

    let nuevoId = generarId eventos
        fecha = obtenerFechaActual

        cat = resumenCategorias eventos
        maxE = eventoMaxCSV eventos
        minE = eventoMinCSV eventos
        dia  = calcularDiaMasActivo eventos

        estadistica = Estadistica
            nuevoId
            fecha
            cat
            maxE
            minE
            dia

    guardarCSV eventos estadistica

    putStrLn (okMsg " Estadística generada correctamente")
    mostrarEstadistica eventos estadistica

    return estadistica



generarId :: [Evento] -> Int
generarId eventos =
    (length eventos * 37 + round (sum (map totalReal eventos))) `mod` 9001

obtenerFechaActual :: Int
obtenerFechaActual = 20260501



totalReal :: Evento -> Float
totalReal e =
    (valor e * fromIntegral (cantidad e)) + impuesto e

limpiarFloat :: Float -> String
limpiarFloat = show



contarPorCategoria :: [Evento] -> [(String, Int)]
contarPorCategoria =
    map (\xs -> (show (categoria (head xs)), length xs))
    . groupBy ((==) `on` (show . categoria))
    . sortBy (compare `on` (show . categoria))

extraerCategoria :: String -> [(String, Int)] -> Int
extraerCategoria cat xs = maybe 0 id (lookup cat xs)

resumenCategorias :: [Evento] -> String
resumenCategorias eventos =
    let cats = contarPorCategoria eventos
    in unlines
        [ "Apartado:      " ++ show (extraerCategoria "Apartado" cats)
        , "Compra:        " ++ show (extraerCategoria "Compra" cats)
        , "Devolución:    " ++ show (extraerCategoria "Devolucion" cats)
        , "Seguimiento:   " ++ show (extraerCategoria "Seguimiento" cats)
        , "Visualización: " ++ show (extraerCategoria "Visualizacion" cats)
        ]



eventoMaxCSV :: [Evento] -> String
eventoMaxCSV eventos =
    let e = maximumBy (compare `on` totalReal) eventos
    in show (idEvento e) ++ "," ++ limpiarFloat (totalReal e)

eventoMinCSV :: [Evento] -> String
eventoMinCSV eventos =
    let e = minimumBy (compare `on` totalReal) eventos
    in show (idEvento e) ++ "," ++ limpiarFloat (totalReal e)



calcularDiaMasActivo :: [Evento] -> String
calcularDiaMasActivo eventos =
    fst (maximumBy (compare `on` snd) (contarDias eventos))

contarDias :: [Evento] -> [(String, Int)]
contarDias eventos =
    map (\xs -> (formatearFecha (timestamp (head xs)), length xs))
    . groupBy ((==) `on` (formatearFecha . timestamp))
    . sortBy (compare `on` (formatearFecha . timestamp))
    $ eventos

formatearFecha :: Int -> String
formatearFecha f =
    let s = show f
    in if length s == 8
        then take 4 s ++ "-" ++ take 2 (drop 4 s) ++ "-" ++ drop 6 s
        else s



guardarCSV :: [Evento] -> Estadistica -> IO ()
guardarCSV eventos e = do

    let cats = contarPorCategoria eventos

        linea =
            show (estId e) ++ "," ++
            show (fechaConsulta e) ++ "," ++
            show (extraerCategoria "Apartado" cats) ++ "," ++
            show (extraerCategoria "Compra" cats) ++ "," ++
            show (extraerCategoria "Devolucion" cats) ++ "," ++
            show (extraerCategoria "Seguimiento" cats) ++ "," ++
            show (extraerCategoria "Visualizacion" cats) ++ "," ++
            eventoMaxCSV eventos ++ "," ++
            eventoMinCSV eventos ++ "," ++
            quitarGuiones (calcularDiaMasActivo eventos)

    appendFile "data/estadisticas.csv" (linea ++ "\n")

quitarGuiones :: String -> String
quitarGuiones = filter (/= '-')



mostrarEstadistica :: [Evento] -> Estadistica -> IO ()
mostrarEstadistica eventos e = do

    putStrLn (titulo "\n╔════════════════════════════════════╗")
    putStrLn (titulo "           ESTADÍSTICAS            ")
    putStrLn (titulo "╚════════════════════════════════════╝")

    putStrLn (okMsg ("ID: " ++ show (estId e)))
    putStrLn (infoMsg ("Fecha consulta: " ++ formatearFecha (fechaConsulta e)))

    putStrLn (subtitulo "\nResumen de categorías:")
    putStrLn (texto (eventosPorCategoria e))

    let maxE = eventoMaxCSV eventos
        minE = eventoMinCSV eventos
        dia  = calcularDiaMasActivo eventos

    putStrLn (amarillo ++ "\n┌── MÁXIMO ──────────────────────────────" ++ reset)
    putStrLn ("│ ID: " ++ formatearLinea maxE)
    putStrLn (amarillo ++ "└───────────────────────────────────────" ++ reset)

    putStrLn (rojo ++ "\n┌── MÍNIMO ──────────────────────────────" ++ reset)
    putStrLn ("│ ID: " ++ formatearLinea minE)
    putStrLn (rojo ++ "└───────────────────────────────────────" ++ reset)

    putStrLn (magenta ++ "\n┌── DÍA MÁS ACTIVO ──────────────────────" ++ reset)
    putStrLn ("│ " ++ safeFecha dia)
    putStrLn (magenta ++ "└───────────────────────────────────────" ++ reset)

    putStrLn (titulo "╚════════════════════════════════════╝")



formatearLinea :: String -> String
formatearLinea s =
    case break (== ',') s of
        (idv, _:resto) ->
            case readMaybe resto :: Maybe Float of
                Just n ->
                    "ID: " ++ idv ++ "  |  Monto: " ++ formatearMonto n
                Nothing ->
                    "ID: " ++ idv ++ "  |  Monto: 0.00"
        _ -> s

safeFecha :: String -> String
safeFecha s =
    let clean = quitarGuiones s
    in if length clean == 8
        then take 4 clean ++ "-" ++ take 2 (drop 4 clean) ++ "-" ++ drop 6 clean
        else clean