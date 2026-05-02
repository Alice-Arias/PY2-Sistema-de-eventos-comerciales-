module UI.AnalisisTemporalUI where

import Types.Evento
import Utils.Colores
import Utils.Formato (formatearMonto)
import Utils.Fecha (formatearFecha)
import Types.Fecha (dayToInt)
import Data.Time (Day)
import System.IO (hFlush, stdout)


imprimirLinea :: String -> IO ()
imprimirLinea = putStrLn

imprimirTitulo :: String -> IO ()
imprimirTitulo txt = do
    imprimirLinea ("\n" ++ titulo linea)
    imprimirLinea (titulo ("        " ++ txt))
    imprimirLinea (titulo linea)
    where
        linea = "========================================"


mostrarMesDiaUI :: String -> Float -> String -> Int -> IO ()
mostrarMesDiaUI mes monto dia cantidad = do
    imprimirTitulo "MES CON MAYOR MONTO"
    imprimirLinea (texto (alinearTexto mes 18) ++ " | " ++ okMsg (formatearMonto monto))

    imprimirTitulo "DÍA MÁS ACTIVO"
    imprimirLinea (texto (alinearTexto dia 18) ++ " | " ++ okMsg (show cantidad ++ " eventos"))


mostrarExtremosUI :: Evento -> Evento -> IO ()
mostrarExtremosUI viejo nuevo = do
    imprimirTitulo "EVENTOS EXTREMOS"
    mostrarEvento "MÁS ANTIGUO" viejo
    mostrarEvento "MÁS RECIENTE" nuevo

mostrarEvento :: String -> Evento -> IO ()
mostrarEvento tituloEvento evento = do
    imprimirLinea ("\n" ++ separador linea)
    imprimirLinea (subtitulo ("        " ++ tituloEvento))
    imprimirLinea (separador linea)

    imprimirLinea (texto ("ID    : " ++ show (idEvento evento)))
    imprimirLinea (texto ("Fecha : " ++ formatearFecha (timestamp evento)))
    where
        linea = "----------------------------------------"


imprimirResumenUI :: [(Day, Day, [Evento])] -> IO ()
imprimirResumenUI grupos = do
    imprimirLinea ("\n" ++ titulo linea)
    imprimirLinea (titulo "        RESUMEN POR INTERVALO DE DÍAS")
    imprimirLinea (titulo linea)

    imprimirLinea $
        subtitulo (ajustarTexto "Rango de Fechas" 40) ++ " | " ++
        subtitulo (ajustarTexto "Cantidad" 8) ++ " | " ++
        subtitulo (ajustarTexto "Monto Total" 18)

    imprimirLinea (separador (replicate 70 '-'))

    mapM_ imprimirGrupo grupos

    where
        linea = "=============================================================="

imprimirGrupo :: (Day, Day, [Evento]) -> IO ()
imprimirGrupo (inicioDay, finDay, grupo) = do
    let inicio = formatearFecha (dayToInt inicioDay)
        fin    = formatearFecha (dayToInt finDay)
        rango  = inicio ++ " - " ++ fin

        cantidad = length grupo
        montoTotal = sum (map total grupo)

    imprimirLinea $
        texto (ajustarTexto rango 40) ++ " | " ++
        texto (ajustarNumero cantidad 8) ++ " | " ++
        okMsg (ajustarTexto (formatearMonto montoTotal) 18)


pedirIntervaloUI :: Int -> IO Int
pedirIntervaloUI maxDias = do
    imprimirLinea ""
    putStr (inputMsg ("Ingrese el intervalo en días (1 - " ++ show maxDias ++ "): "))
    hFlush stdout   

    input <- getLine

    case reads input :: [(Int, String)] of
        [(dias, "")] | dias >= 1 && dias <= maxDias -> return dias
        [(dias, "")] -> do
            imprimirLinea (errorMsg "Fuera de rango.")
            pedirIntervaloUI maxDias
        _ -> do
            imprimirLinea (errorMsg "Entrada inválida.")
            pedirIntervaloUI maxDias


ajustarTexto :: String -> Int -> String
ajustarTexto txt ancho = take ancho (txt ++ repeat ' ')

ajustarNumero :: Int -> Int -> String
ajustarNumero n ancho =
    let txt = show n
    in replicate (ancho - length txt) ' ' ++ txt

alinearTexto :: String -> Int -> String
alinearTexto texto ancho =
    texto ++ replicate (ancho - length texto) ' '