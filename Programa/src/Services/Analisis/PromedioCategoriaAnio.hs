module Services.Analisis.PromedioCategoriaAnio where

import Types.Modelos
import Utils.Calculos
import UI.Interfaz
import Utils.Colores

--------------------------------------------------------------------------------
-- Nombre: promedioCategoriaPorAnio
-- Entrada: lista de eventos del sistema
-- Salida:
--   Muestra en pantalla el promedio de dinero por categoría, separado por año
-- Restricciones:
--   - Los eventos deben tener fecha válida para poder agrupar por año
--------------------------------------------------------------------------------
promedioCategoriaPorAnio :: [Evento] -> IO ()
promedioCategoriaPorAnio eventos = do

    let 
        aniosDisponibles = obtenerAnios eventos

        categoriasDisponibles = obtenerCategorias eventos

    putStrLn (titulo "\n================================")
    putStrLn (titulo " PROMEDIO POR CATEGORÍA POR AÑO")
    putStrLn (titulo "================================")

    mapM_ (mostrarPromediosPorAnio categoriasDisponibles eventos) aniosDisponibles

    putStrLn (titulo "================================")


--------------------------------------------------------------------------------
-- Nombre: mostrarPromediosPorAnio
-- Entrada:
--   categoriasDisponibles: lista de categorías del sistema
--   eventos: lista de eventos del sistema
--   anio: año específico a analizar
-- Salida:
--   Muestra en pantalla el promedio por categoría en ese año
-- Restricciones:
--   - El año debe existir en los eventos
--------------------------------------------------------------------------------
mostrarPromediosPorAnio :: [Categoria] -> [Evento] -> Integer -> IO ()
mostrarPromediosPorAnio categoriasDisponibles eventos anio = do

    putStrLn (titulo ("\nAÑO: " ++ show anio))
    putStrLn (separador "--------------------------------------------")
    putStrLn (subtitulo "CATEGORÍA        | PROMEDIO")
    putStrLn (separador "--------------------------------------------")

    mapM_ (\categoriaActual -> let promedio = promedioCategoriaAnioCalc eventos anio categoriaActual in mostrarPromedioCategoria categoriaActual promedio) categoriasDisponibles

    putStrLn (separador "--------------------------------------------")