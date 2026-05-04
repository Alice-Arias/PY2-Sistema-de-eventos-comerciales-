module Utils.Formato where

import Numeric (showFFloat)
import Data.List (intercalate)
import Types.Modelos

--------------------------------------------------------------------------------
-- Nombre: formatearMonto
-- Entrada:
--   n: valor numérico en flotante
-- Salida:
--   monto formateado en colones con 2 decimales y separadores
-- Restricciones:
--   - Agrega prefijo CRC
--------------------------------------------------------------------------------
formatearMonto :: Float -> String
formatearMonto monto =
    "CRC " ++ formatearConComas (showFFloat (Just 2) monto "")

--------------------------------------------------------------------------------
-- Nombre: formatearConComas
-- Entrada:
--   str: número en formato String
-- Salida:
--   número con separadores de miles
-- Restricciones:
--   - Solo formateo de texto
--------------------------------------------------------------------------------
formatearConComas :: String -> String
formatearConComas numeroTexto =
    let
        (parteEntera, parteDecimal) = span (/='.') numeroTexto
        gruposDigitos = agrupar3 (reverse parteEntera)
    in
        reverse (intercalate "," gruposDigitos) ++ parteDecimal

--------------------------------------------------------------------------------
-- Nombre: agrupar3
-- Entrada:
--   xs: cadena de caracteres numéricos
-- Salida:
--   lista de grupos de 3 caracteres
-- Restricciones:
--   - Agrupa de 3 en 3
--------------------------------------------------------------------------------
agrupar3 :: String -> [String]
agrupar3 texto =
    case texto of
        [] -> []
        _  -> take 3 texto : agrupar3 (drop 3 texto)
--------------------------------------------------------------------------------
-- FORMATO VISUAL
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: ajustarTexto
-- Entrada:
--   txt: texto base
--   n: tamaño máximo
-- Salida:
--   texto ajustado al ancho definido
-- Restricciones:
--   - Rellena con espacios si es necesario
--------------------------------------------------------------------------------
ajustarTexto :: String -> Int -> String
ajustarTexto texto longitud = take longitud (texto ++ repeat ' ')

--------------------------------------------------------------------------------
-- Nombre: ajustarNumero
-- Entrada:
--   n: número entero
--   ancho: tamaño de columna
-- Salida:
--   número alineado a la derecha
-- Restricciones:
--   - Completa con espacios a la izquierda
--------------------------------------------------------------------------------
ajustarNumero :: Int -> Int -> String
ajustarNumero numero anchoTotal =
    let textoNumero = show numero
    in replicate (anchoTotal - length textoNumero) ' ' ++ textoNumero

--------------------------------------------------------------------------------
-- Nombre: alinearTexto
-- Entrada:
--   txt: texto base
--   n: ancho de columna
-- Salida:
--   texto alineado a la izquierda
-- Restricciones:
--   - Completa con espacios a la derecha
--------------------------------------------------------------------------------
alinearTexto :: String -> Int -> String
alinearTexto texto ancho = texto ++ replicate (ancho - length texto) ' '


--------------------------------------------------------------------------------
-- Nombre: obtenerPromedio
-- Entrada:
--   cat: categoría buscada
--   lista: lista de (categoría, promedio)
-- Salida:
--   promedio asociado a la categoría
-- Restricciones:
--   - Devuelve 0 si no existe
--------------------------------------------------------------------------------
obtenerPromedio :: Categoria -> [(Categoria, Float)] -> Float
obtenerPromedio categoriaBuscada listaPromedios =
    case lookup categoriaBuscada listaPromedios of
        Just promedio -> promedio
        Nothing -> 0

--------------------------------------------------------------------------------
-- Nombre: col
-- Entrada:
--   txt: texto base
--   n: tamaño máximo
-- Salida:
--   texto recortado o ajustado
-- Restricciones:
--   - Solo formateo visual
--------------------------------------------------------------------------------
col :: String -> Int -> String
col texto ancho = take ancho (texto ++ repeat ' ')