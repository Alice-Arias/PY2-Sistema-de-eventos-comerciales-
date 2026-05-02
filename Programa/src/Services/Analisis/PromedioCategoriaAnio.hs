module Services.Analisis.PromedioCategoriaAnio where

import Types.Evento
import Types.Categoria
import Utils.Fecha (extraerAnio)
import Utils.Formato (formatearMonto)
import Utils.Colores
import Data.List (nub, sort)

promedioCategoriaPorAnio :: [Evento] -> IO ()
promedioCategoriaPorAnio eventos = do

    let listaAnios = obtenerAnios eventos
        listaCategorias = obtenerCategorias eventos

    putStrLn (titulo "\n================================")
    putStrLn (titulo " PROMEDIO POR CATEGORÍA POR AÑO")
    putStrLn (titulo "================================")

    mapM_ (mostrarPorAnio listaCategorias eventos) listaAnios

    putStrLn (titulo "================================")

obtenerAnios :: [Evento] -> [Integer]
obtenerAnios eventos = sort (nub (map extraerAnio eventos))


obtenerCategorias :: [Evento] -> [Categoria]
obtenerCategorias eventos = nub (map categoria eventos)


mostrarPorAnio :: [Categoria] -> [Evento] -> Integer -> IO ()
mostrarPorAnio categorias eventos anio = do

    putStrLn (titulo ("\nAÑO: " ++ show anio))

    putStrLn (separador "--------------------------------------------")
    putStrLn (subtitulo "CATEGORÍA        | PROMEDIO")
    putStrLn (separador "--------------------------------------------")

    mapM_ (mostrarCategoria anio eventos) categorias

    putStrLn (separador "--------------------------------------------")


mostrarCategoria :: Integer -> [Evento] -> Categoria -> IO ()
mostrarCategoria anio eventos cat =

    let eventosFiltrados = filter (\e ->categoria e == cat && extraerAnio e == anio ) eventos

        sumaTotal = sum (map total eventosFiltrados)

        cantidad = length eventosFiltrados

        promedio =
            if cantidad == 0
            then 0
            else sumaTotal / fromIntegral cantidad

    in putStrLn(texto (ajustarTexto (show cat) 16) ++ texto " | "++ okMsg (formatearMonto promedio))


ajustarTexto :: String -> Int -> String
ajustarTexto txt n = take n (txt ++ repeat ' ') 