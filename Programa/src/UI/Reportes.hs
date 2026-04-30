module UI.Reportes where

import Types.Evento
import Types.Categoria
import Data.List (nub)
import Core.Impuestos
import Core.Etiquetas
import Utils.Calculos
import Core.Promedios

reporteImpuestos :: [Evento] -> IO ()
reporteImpuestos eventos = do

    let eventosConTotal = actualizarTotales eventos

        totalEventos = length eventos
        totalCompra = length (filter (\e -> categoria e == Compra) eventos)

        totalImpuestos =length (filter (\e -> categoria e == Compra && impuesto e > 0) eventosConTotal)

        promedio = promedioCompras eventosConTotal

    putStrLn "===== REPORTE IMPUESTOS ====="
    putStrLn ("Total eventos: " ++ show totalEventos)
    putStrLn ("Compras: " ++ show totalCompra)
    putStrLn ("Con impuesto: " ++ show totalImpuestos)

    putStrLn "==============================="


reporteEtiquetas :: [Evento] -> IO ()
reporteEtiquetas eventos = do

    let eventosEtiqueta = etiquetarAltoValor eventos
        categorias = nub (map categoria eventos)
        promedios = calcularPromedios eventos
        totalGeneral = length eventos
        totalSobrePromedio = length (filter etiqueta eventosEtiqueta)

    putStrLn "================= REPORTE ETIQUETAS ================="
    putStrLn ("Total de eventos: " ++ show totalGeneral)
    putStrLn ("Total sobre promedio: " ++ show totalSobrePromedio)
    putStrLn ""

    mapM_ (\cat ->

        let eventosCat = filter (\e -> categoria e == cat) eventosEtiqueta
            cantidadTotal = length eventosCat
            sobrePromedio = length (filter etiqueta eventosCat)
            promedio = obtenerPromedio cat promedios

        in putStrLn (
            show cat ++
            " | Total: " ++ show cantidadTotal ++
            " | Promedio: " ++ show promedio ++
            " | Sobre promedio: " ++ show sobrePromedio
        )

        ) categorias

    putStrLn "================================================"

obtenerPromedio :: Categoria -> [(Categoria, Float)] -> Float
obtenerPromedio cat lista =
    case lookup cat lista of
        Just p  -> p
        Nothing -> 0

reporteCompleto :: [Evento] -> IO ()
reporteCompleto eventos = do
    let eventosFinales = actualizarTotales eventos

        totalImp = length (filter (\e -> categoria e == Compra && impuesto e > 0) eventosFinales)
        totalEti = length (filter etiqueta eventosFinales)

        promedio = promedioCompras eventosFinales

    putStrLn "===== REPORTE COMPLETO ====="
    putStrLn ("Eventos: " ++ show (length eventosFinales))
    putStrLn ("Impuestos: " ++ show totalImp)
    putStrLn ("Sobre promedio: " ++ show totalEti)
    putStrLn "==============================="
