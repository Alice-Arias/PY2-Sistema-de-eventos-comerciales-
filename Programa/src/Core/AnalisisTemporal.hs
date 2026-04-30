module Core.AnalisisTemporal where

import Types.Evento
import Services.Analisis.MesMayorMonto
import Services.Analisis.DiaSemanaActivo
import Services.Analisis.EventosExtremos
import Services.Analisis.ResumenIntervalos
import Utils.Formato (formatearMonto)
import Utils.Fecha (formatearFecha)


-- =========================
-- MES + DÍA
-- =========================

analisisMesDia :: [Evento] -> IO ()
analisisMesDia eventos = do

    let (mesTop, montoMesTop) = mesConMayorMonto eventos
    let (diaTop, cantidadDiaTop) = diaMasActivo eventos

    putStrLn "\n========================================"
    putStrLn "        MES CON MAYOR MONTO"
    putStrLn "========================================"

    putStrLn (alinearTexto mesTop 18 ++ "| " ++ formatearMonto montoMesTop)

    putStrLn "\n========================================"
    putStrLn "        DÍA MÁS ACTIVO"
    putStrLn "========================================"

    putStrLn (alinearTexto diaTop 18 ++ "| " ++ show cantidadDiaTop ++ " eventos")


-- =========================
-- EXTREMOS
-- =========================

analisisExtremos :: [Evento] -> IO ()
analisisExtremos eventos = do

    let (eventoViejo, eventoNuevo) = eventosExtremos eventos

    putStrLn "\n========================================"
    putStrLn "        EVENTOS EXTREMOS"
    putStrLn "========================================"

    mostrarEvento "MÁS ANTIGUO" eventoViejo
    mostrarEvento "MÁS RECIENTE" eventoNuevo


mostrarEvento :: String -> Evento -> IO ()
mostrarEvento titulo evento = do

    putStrLn "\n────────────────────────────────────────"
    putStrLn ("             " ++ titulo)
    putStrLn "────────────────────────────────────────"

    putStrLn ("ID    : " ++ show (idEvento evento))
    putStrLn ("Fecha : " ++ formatearFecha (timestamp evento))


-- =========================
-- RESUMEN
-- =========================

analisisResumen :: [Evento] -> IO ()
analisisResumen eventos = do

    let resumen = resumenIntervalos eventos

    putStrLn "\n========================================"
    putStrLn "        RESUMEN POR MES"
    putStrLn "========================================"

    mapM_ imprimirLineaResumen resumen


imprimirLineaResumen :: (String, Int, Float) -> IO ()
imprimirLineaResumen (mes, cantidad, monto) = do

    putStrLn (
        alinearTexto mes 17
        ++ "| Cant: " ++ alinearNumero cantidad
        ++ " | " ++ formatearMonto monto)


-- =========================
-- HELPERS
-- =========================

alinearTexto :: String -> Int -> String
alinearTexto texto ancho =
    texto ++ replicate (ancho - length texto) ' '

alinearNumero :: Int -> String
alinearNumero n =
    let txt = show n
    in replicate (3 - length txt) ' ' ++ txt