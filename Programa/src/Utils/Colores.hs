module Utils.Colores where

--------------------------------------------------------------------------------
-- Nombre: reset
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI para resetear color en consola
-- Restricciones:
--   - Uso exclusivo para formato de texto
--------------------------------------------------------------------------------
reset :: String
reset = "\ESC[0m"

--------------------------------------------------------------------------------
-- COLORES BASE
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: rojo
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI color rojo
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
rojo :: String
rojo = "\ESC[31m"

--------------------------------------------------------------------------------
-- Nombre: verde
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI color verde
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
verde :: String
verde = "\ESC[32m"

--------------------------------------------------------------------------------
-- Nombre: amarillo
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI color amarillo
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
amarillo :: String
amarillo = "\ESC[33m"

--------------------------------------------------------------------------------
-- Nombre: azul
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI color azul
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
azul :: String
azul = "\ESC[34m"

--------------------------------------------------------------------------------
-- Nombre: magenta
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI color magenta
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
magenta :: String
magenta = "\ESC[35m"

--------------------------------------------------------------------------------
-- Nombre: cyan
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI color cyan
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
cyan :: String
cyan = "\ESC[36m"

--------------------------------------------------------------------------------
-- Nombre: blanco
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI color blanco
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
blanco :: String
blanco = "\ESC[37m"

--------------------------------------------------------------------------------
-- ESTILOS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: bold
-- Entrada:
--   Ninguna
-- Salida:
--   Código ANSI texto en negrita
-- Restricciones:
--   - Uso en consola
--------------------------------------------------------------------------------
bold :: String
bold = "\ESC[1m"

--------------------------------------------------------------------------------
-- UTILIDADES DE COLOR
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: color
-- Entrada:
--   c: código de color ANSI
--   txt: texto a colorear
-- Salida:
--   texto con color aplicado
-- Restricciones:
--   - Aplica reset al final
--------------------------------------------------------------------------------
color :: String -> String -> String
color c txt = c ++ txt ++ reset

--------------------------------------------------------------------------------
-- Nombre: colorBold
-- Entrada:
--   c: código de color ANSI
--   txt: texto a estilizar
-- Salida:
--   texto en negrita con color
-- Restricciones:
--   - Aplica reset al final
--------------------------------------------------------------------------------
colorBold :: String -> String -> String
colorBold c txt = bold ++ c ++ txt ++ reset

--------------------------------------------------------------------------------
-- MENSAJES DE UI
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: titulo
-- Entrada:
--   txt: texto del título
-- Salida:
--   texto en estilo título
-- Restricciones:
--   - Usa color azul y negrita
--------------------------------------------------------------------------------
titulo :: String -> String
titulo txt = colorBold azul txt

--------------------------------------------------------------------------------
-- Nombre: subtitulo
-- Entrada:
--   txt: texto del subtítulo
-- Salida:
--   texto en estilo subtítulo
-- Restricciones:
--   - Usa color magenta y negrita
--------------------------------------------------------------------------------
subtitulo :: String -> String
subtitulo txt = colorBold magenta txt

--------------------------------------------------------------------------------
-- Nombre: opcion
-- Entrada:
--   txt: texto de opción
-- Salida:
--   texto en color amarillo
-- Restricciones:
--   - Solo formato visual
--------------------------------------------------------------------------------
opcion :: String -> String
opcion txt = color amarillo txt

--------------------------------------------------------------------------------
-- Nombre: texto
-- Entrada:
--   txt: texto normal
-- Salida:
--   texto en color blanco
-- Restricciones:
--   - Uso general
--------------------------------------------------------------------------------
texto :: String -> String
texto txt = color blanco txt

--------------------------------------------------------------------------------
-- Nombre: inputMsg
-- Entrada:
--   txt: texto de entrada
-- Salida:
--   texto en color cyan
-- Restricciones:
--   - Para prompts de usuario
--------------------------------------------------------------------------------
inputMsg :: String -> String
inputMsg txt = color cyan txt

--------------------------------------------------------------------------------
-- Nombre: okMsg
-- Entrada:
--   txt: mensaje de éxito
-- Salida:
--   texto en verde negrita
-- Restricciones:
--   - Indica operación exitosa
--------------------------------------------------------------------------------
okMsg :: String -> String
okMsg txt = colorBold verde txt

--------------------------------------------------------------------------------
-- Nombre: errorMsg
-- Entrada:
--   txt: mensaje de error
-- Salida:
--   texto en rojo negrita
-- Restricciones:
--   - Indica error del sistema
--------------------------------------------------------------------------------
errorMsg :: String -> String
errorMsg txt = colorBold rojo txt

--------------------------------------------------------------------------------
-- Nombre: warningMsg
-- Entrada:
--   txt: mensaje de advertencia
-- Salida:
--   texto en amarillo
-- Restricciones:
--   - Uso informativo
--------------------------------------------------------------------------------
warningMsg :: String -> String
warningMsg txt = color amarillo txt

--------------------------------------------------------------------------------
-- Nombre: separador
-- Entrada:
--   txt: texto separador
-- Salida:
--   texto en azul
-- Restricciones:
--   - Solo visual
--------------------------------------------------------------------------------
separador :: String -> String
separador txt = color azul txt

--------------------------------------------------------------------------------
-- Nombre: infoMsg
-- Entrada:
--   txt: mensaje informativo
-- Salida:
--   texto en azul
-- Restricciones:
--   - Uso informativo general
--------------------------------------------------------------------------------
infoMsg :: String -> String
infoMsg txt = color azul txt