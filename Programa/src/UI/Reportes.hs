module UI.Reportes where

import Types.Evento
import Types.Categoria
import Data.List (nub)
import Core.Impuestos
import Core.Etiquetas
import Utils.Calculos
import Core.Promedios
import Utils.Formato (formatearMonto)
import Utils.Colores


reporteImpuestos :: [Evento] -> IO ()
reporteImpuestos eventos = do

    let eventosConTotal = actualizarTotales eventos

        totalEventos = length eventos
        totalCompra = length (filter (\e -> categoria e == Compra) eventos)
        totalImpuestos = length (filter (\e -> categoria e == Compra && impuesto e > 0) eventosConTotal)

    putStrLn (titulo "\n══════════════════════════════════════")
    putStrLn (subtitulo "        REPORTE DE IMPUESTOS")
    putStrLn (titulo "══════════════════════════════════════")

    putStrLn (texto ("Total eventos   : " ++ show totalEventos))
    putStrLn (texto ("Compras         : " ++ show totalCompra))
    putStrLn (okMsg ("Con impuesto    : " ++ show totalImpuestos))

    putStrLn (titulo "══════════════════════════════════════")



reporteEtiquetas :: [Evento] -> IO ()
reporteEtiquetas eventos = do

    let eventosEtiqueta = etiquetarAltoValor eventos
        categorias = nub (map categoria eventos)
        promedios = calcularPromedios eventos

        totalGeneral = length eventos
        totalSobrePromedio = length (filter etiqueta eventosEtiqueta)

    putStrLn (titulo "\n════════════════════════════════════════")
    putStrLn (subtitulo "        REPORTE DE ETIQUETAS")
    putStrLn (titulo "════════════════════════════════════════")

    putStrLn (texto ("Total eventos      : " ++ show totalGeneral))
    putStrLn (okMsg ("Sobre promedio     : " ++ show totalSobrePromedio))
    putStrLn ""

    putStrLn (colorBold magenta
        (ajustarTexto "Categoría" 15 ++ " | "
        ++ ajustarTexto "Total" 7 ++ " | "
        ++ ajustarTexto "Promedio" 18 ++ " | "
        ++ "Sobre"))

    putStrLn (separador (replicate 65 '-'))

    mapM_ (\cat ->
        let eventosCat = filter (\e -> categoria e == cat) eventosEtiqueta
            cantidadTotal = length eventosCat
            sobrePromedio = length (filter etiqueta eventosCat)
            promedio = obtenerPromedio cat promedios

        in putStrLn (texto (
            ajustarTexto (show cat) 15 ++ " | "
        ++ ajustarNumero cantidadTotal 7 ++ " | "
        ++ ajustarTexto (formatearMonto promedio) 18 ++ " | "
        ++ ajustarNumero sobrePromedio 6)) ) categorias

    putStrLn (titulo "════════════════════════════════════════")


reporteCompleto :: [Evento] -> IO ()
reporteCompleto eventos = do

    let eventosFinales = actualizarTotales eventos

        totalImp = length (filter (\e -> categoria e == Compra && impuesto e > 0) eventosFinales)
        totalEti = length (filter etiqueta eventosFinales)

    putStrLn (titulo "\n══════════════════════════════════════")
    putStrLn (subtitulo "        REPORTE COMPLETO")
    putStrLn (titulo "══════════════════════════════════════")

    putStrLn (texto ("Eventos         : " ++ show (length eventosFinales)))
    putStrLn (okMsg ("Impuestos       : " ++ show totalImp))
    putStrLn (okMsg ("Sobre promedio  : " ++ show totalEti))

    putStrLn (titulo "══════════════════════════════════════")

obtenerPromedio :: Categoria -> [(Categoria, Float)] -> Float
obtenerPromedio cat lista =
    case lookup cat lista of
        Just p  -> p
        Nothing -> 0

ajustarTexto :: String -> Int -> String
ajustarTexto txt ancho =
    take ancho (txt ++ repeat ' ')

ajustarNumero :: Int -> Int -> String
ajustarNumero n ancho =
    let txt = show n
    in replicate (ancho - length txt) ' ' ++ txt