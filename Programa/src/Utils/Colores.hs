module Utils.Colores where

reset :: String
reset = "\ESC[0m"

-- COLORES BASE
rojo, verde, amarillo, azul, magenta, cyan, blanco :: String
rojo     = "\ESC[31m"
verde    = "\ESC[32m"
amarillo = "\ESC[33m"
azul     = "\ESC[34m"
magenta  = "\ESC[35m"
cyan     = "\ESC[36m"
blanco   = "\ESC[37m"

-- ESTILOS
bold :: String
bold = "\ESC[1m"


color :: String -> String -> String
color c txt = c ++ txt ++ reset

colorBold :: String -> String -> String
colorBold c txt = bold ++ c ++ txt ++ reset


-- Títulos principales (menos chillones que cyan)
titulo :: String -> String
titulo txt = colorBold azul txt

-- Subtítulos
subtitulo :: String -> String
subtitulo txt = colorBold magenta txt

-- Opciones del menú
opcion :: String -> String
opcion txt = color amarillo txt

-- Texto normal
texto :: String -> String
texto txt = color blanco txt

-- Input del usuario (no tan fuerte)
inputMsg :: String -> String
inputMsg txt = color cyan txt

-- Éxito
okMsg :: String -> String
okMsg txt = colorBold verde txt

-- Error
errorMsg :: String -> String
errorMsg txt = colorBold rojo txt

-- Advertencia
warningMsg :: String -> String
warningMsg txt = color amarillo txt

-- Separadores
separador :: String -> String
separador txt = color azul txt

infoMsg :: String -> String
infoMsg txt = color azul txt