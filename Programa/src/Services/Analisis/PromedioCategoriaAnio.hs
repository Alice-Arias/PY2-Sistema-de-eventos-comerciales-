module Services.Analisis.PromedioCategoriaAnio where

import Types.Modelos
import Utils.Calculos
import UI.Interfaz
import Utils.Colores

--------------------------------------------------------------------------------
-- Nombre: promedioCategoriaPorAnio
--
-- Objetivo: muestra el promedio de valor por categoría separado por año
--
-- Entradas: lista de eventos del sistema
--
-- Salida: IO () con resultados impresos en pantalla
--
-- Restricciones:
--   - los eventos deben tener fecha válida para agrupar por año
--------------------------------------------------------------------------------
promedioCategoriaPorAnio :: [Evento] -> IO ()
promedioCategoriaPorAnio eventos = do

    let 
        aniosDisponibles = obtenerAnios eventos

        categoriasDisponibles = obtenerCategorias eventos

    putStrLn (titulo "\n════════════════════════════════════════")
    putStrLn (magenta2 "    PROMEDIO POR CATEGORÍA POR AÑO")
    putStrLn (titulo "════════════════════════════════════════")

    mapM_ (mostrarPromediosPorAnio categoriasDisponibles eventos) aniosDisponibles

    putStrLn (titulo "════════════════════════════════════════")


--------------------------------------------------------------------------------
-- Nombre: mostrarPromediosPorAnio
--
-- Objetivo: muestra el promedio por categoría para un año específico
--
-- Entradas:
--   - lista de categorías
--   - lista de eventos
--   - año a analizar
--
-- Salida: IO () con tabla de promedios
--
-- Restricciones:
--   - el año debe existir en los eventos
--------------------------------------------------------------------------------
mostrarPromediosPorAnio :: [Categoria] -> [Evento] -> Integer -> IO ()
mostrarPromediosPorAnio categoriasDisponibles eventos anio = do

    putStrLn (opcion ("\nAÑO: " ++ show anio))
    putStrLn (separador "--------------------------------------------")
    putStrLn (subtitulo "CATEGORÍA        | PROMEDIO")
    putStrLn (separador "--------------------------------------------")

    mapM_ (\categoriaActual -> let promedio = promedioCategoriaAnioCalc eventos anio categoriaActual in mostrarPromedioCategoria categoriaActual promedio) categoriasDisponibles

    putStrLn (separador "--------------------------------------------")