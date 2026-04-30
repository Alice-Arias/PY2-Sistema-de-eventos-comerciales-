module Core.AnalisisTemporal where

import Types.Evento
import Services.Analisis.MesMayorMonto
import Services.Analisis.DiaSemanaActivo
import Services.Analisis.EventosExtremos
import Utils.Formato (formatearMonto)
import Utils.Colores
import Data.Time (Day, addDays, diffDays)
import Types.Fecha (intToDay)
import Utils.Fecha (formatearFecha)
import Data.List (sortOn)
import System.IO (hFlush, stdout)

-- =========================
-- MES + DÍA
-- =========================

analisisMesDia :: [Evento] -> IO ()
analisisMesDia eventos = do

    let (mesTop, montoMesTop) = mesConMayorMonto eventos
    let (diaTop, cantidadDiaTop) = diaMasActivo eventos

    putStrLn ("\n" ++ titulo "========================================")
    putStrLn (titulo "        MES CON MAYOR MONTO")
    putStrLn (titulo "========================================")

    putStrLn (
        texto (alinearTexto mesTop 18) ++
        " | " ++ okMsg (formatearMonto montoMesTop))

    putStrLn ("\n" ++ titulo "========================================")
    putStrLn (titulo "        DÍA MÁS ACTIVO")
    putStrLn (titulo "========================================")

    putStrLn (
        texto (alinearTexto diaTop 18) ++
        " | " ++ okMsg (show cantidadDiaTop ++ " eventos"))

-- =========================
-- PEDIR INTERVALO
-- =========================
pedirIntervalo :: Int -> IO Int
pedirIntervalo maxDias = do
    putStrLn ""
    putStr (inputMsg ("Ingrese el intervalo en días (1 - " ++ show maxDias ++ "): "))
    hFlush stdout   

    input <- getLine

    case reads input :: [(Int, String)] of
        [(dias, "")] | dias >= 1 && dias <= maxDias ->
            return dias

        [(dias, "")] -> do
            putStrLn (errorMsg "Fuera de rango.")
            pedirIntervalo maxDias

        _ -> do
            putStrLn (errorMsg "Entrada inválida.")
            pedirIntervalo maxDias

-- =========================
-- EXTREMOS
-- =========================

analisisExtremos :: [Evento] -> IO ()
analisisExtremos eventos = do

    let (eventoViejo, eventoNuevo) = eventosExtremos eventos

    putStrLn ("\n" ++ titulo "========================================")
    putStrLn (titulo "        EVENTOS EXTREMOS")
    putStrLn (titulo "========================================")

    mostrarEvento "MÁS ANTIGUO" eventoViejo
    mostrarEvento "MÁS RECIENTE" eventoNuevo

mostrarEvento :: String -> Evento -> IO ()
mostrarEvento tituloEvento evento = do

    putStrLn ("\n" ++ separador "----------------------------------------")
    putStrLn (subtitulo ("        " ++ tituloEvento))
    putStrLn (separador "----------------------------------------")

    putStrLn (texto ("ID    : " ++ show (idEvento evento)))
    putStrLn (texto ("Fecha : " ++ formatearFecha (timestamp evento)))

-- =========================
-- RESUMEN POR INTERVALOS
-- =========================

analisisResumen :: [Evento] -> IO ()
analisisResumen [] = putStrLn (errorMsg "No hay eventos.")
analisisResumen eventos = do

    let (eventoViejo, eventoNuevo) = eventosExtremos eventos
    let fechaMin = timestamp eventoViejo
    let fechaMax = timestamp eventoNuevo

    let diasDisponibles =
            fromIntegral (diffDays (intToDay fechaMax) (intToDay fechaMin))

    intervalo <- pedirIntervalo diasDisponibles

    let grupos = agruparPorIntervalo intervalo eventos

    putStrLn ("\n" ++ titulo "==============================================================")
    putStrLn (titulo "        RESUMEN POR INTERVALO DE DÍAS")
    putStrLn (titulo "==============================================================")

    putStrLn (
        subtitulo (ajustarTexto "Rango de Fechas" 40) ++ " | " ++
        subtitulo (ajustarTexto "Cantidad" 8) ++ " | " ++
        subtitulo (ajustarTexto "Monto Total" 18))

    putStrLn (separador (replicate 70 '-'))

    mapM_ imprimirGrupo grupos

-- =========================
-- AGRUPACIÓN
-- =========================

agruparPorIntervalo :: Int -> [Evento] -> [[Evento]]
agruparPorIntervalo _ [] = []
agruparPorIntervalo dias eventos =
    let ordenados = sortOn timestamp eventos
        inicio = intToDay (timestamp (head ordenados))
    in agruparPorDias inicio dias ordenados

agruparPorDias :: Day -> Int -> [Evento] -> [[Evento]]
agruparPorDias _ _ [] = []
agruparPorDias inicio dias eventos =
    let fin = addDays (fromIntegral dias) inicio
        (grupo, resto) =
            span (\e -> intToDay (timestamp e) < fin) eventos
    in if null grupo
        then agruparPorDias fin dias resto
        else grupo : agruparPorDias fin dias resto

-- =========================
-- IMPRESIÓN
-- =========================

imprimirGrupo :: [Evento] -> IO ()
imprimirGrupo [] = return ()
imprimirGrupo grupo = do
    let inicio = formatearFecha (timestamp (head grupo))
        fin = formatearFecha (timestamp (last grupo))
        rango = inicio ++ " - " ++ fin

        cantidad = length grupo
        montoTotal = sum (map total grupo)

    putStrLn (
        texto (ajustarTexto rango 40) ++ " | " ++
        texto (ajustarNumero cantidad 8) ++ " | " ++
        okMsg (ajustarTexto (formatearMonto montoTotal) 18))

-- =========================
-- UTILS
-- =========================

ajustarTexto :: String -> Int -> String
ajustarTexto txt ancho =
    take ancho (txt ++ repeat ' ')

ajustarNumero :: Int -> Int -> String
ajustarNumero n ancho =
    let txt = show n
    in replicate (ancho - length txt) ' ' ++ txt

alinearTexto :: String -> Int -> String
alinearTexto texto ancho =
    texto ++ replicate (ancho - length texto) ' '