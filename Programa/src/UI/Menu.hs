module UI.Menu where


-- Nombre: mostrarBienvenida
-- Descripción: Muestra la pantalla de bienvenida del sistema al iniciar la aplicación.
-- Entradas: No recibe parámetros.
-- Salidas: Mensajes en consola (IO ()).
-- Validaciones: Ninguna, es únicamente presentación visual.
mostrarBienvenida :: IO ()
mostrarBienvenida = do
    putStrLn "========================================"
    putStrLn "      SISTEMA DE EVENTOS v1.0"
    putStrLn "========================================"
    putStrLn "   Bienvenido al sistema principal"
    putStrLn "   Analítica y gestión de eventos"
    putStrLn "========================================"
    putStrLn ""

-- Nombre: mostrarMenu
-- Descripción: Despliega el menú principal del sistema con las opciones disponibles para el usuario.
-- Entradas: No recibe parámetros.
-- Salidas: Menú impreso en consola (IO ()).
-- Validaciones: No valida entrada, solo muestra opciones.
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "========================================"
    putStrLn "              MENÚ PRINCIPAL"
    putStrLn "========================================"
    putStrLn "1. Transformación de eventos"
    putStrLn "2. Análisis de datos"
    putStrLn "3. Análisis temporal"
    putStrLn "4. Búsqueda"
    putStrLn "5. Estadísticas"
    putStrLn "6. Salir"
    putStrLn "========================================"
    putStrLn "Seleccione una opción: "

-- Nombre: limpiarPantalla
-- Descripción: Limpia la consola simulando un borrado de pantalla mediante secuencias de escape.
-- Entradas: No recibe parámetros.
-- Salidas: Consola limpia visualmente (IO ()).
-- Validaciones: Puede no funcionar en todos los sistemas operativos o terminales.
limpiarPantalla :: IO ()
limpiarPantalla = putStrLn "\ESC[2J"