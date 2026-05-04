module UI.Interfaz
(
    -- menú
    mostrarBienvenida,
    mostrarMenu,
    pedirOpcionUsuario,

    -- análisis temporal
    mostrarMesDiaUI,
    mostrarExtremosUI,
    imprimirResumenUI,
    pedirIntervaloUI,

    -- reportes
    reporteCompleto,
    reporteImpuestos,
    reporteEtiquetas,

    -- sistema
    mostrarError,
    mostrarSalidaSistema,
    mostrarResumenActualizacion,
    mostrarEstadistica,
    formatearLinea,
    safeFecha,
    imprimirEncabezado,
    imprimirTabla,
    imprimirFila,
    imprimirResumen,

    mostrarPromedioCategoria
) where

import Types.Modelos
import Types.Fecha
import Data.Time 
import System.IO 

import Utils.Colores
import Utils.Formato 
import Data.List 

import Core.Impuestos
import Core.Etiquetas
import Core.Promedios

import Utils.Calculos

import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- MENÚ PRINCIPAL
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: mostrarBienvenida
-- Entrada: ninguna
-- Salida: imprime mensaje de bienvenida en pantalla
-- Restricciones: solo salida por consola
--------------------------------------------------------------------------------
mostrarBienvenida :: IO ()
mostrarBienvenida = do
    putStrLn ""
    putStrLn (separador "========================================")
    putStrLn (titulo "        SISTEMA DE EVENTOS")
    putStrLn (separador "========================================")
    putStrLn (texto "        Bienvenido al sistema")
    putStrLn ""

--------------------------------------------------------------------------------
-- Nombre: mostrarMenuPrincipal
-- Entrada: ninguna
-- Salida: imprime opciones del menú principal
-- Restricciones: solo interfaz, no lógica de negocio
--------------------------------------------------------------------------------
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn (separador "========================================")
    putStrLn (titulo "            MENÚ PRINCIPAL")
    putStrLn (separador "========================================")

    putStrLn (opcion " 1. Transformación de eventos")
    putStrLn (opcion " 2. Análisis de datos")
    putStrLn (opcion " 3. Análisis temporal")
    putStrLn (opcion " 4. Búsqueda")
    putStrLn (opcion " 5. Estadísticas")
    putStrLn (opcion " 6. Salir")

    putStrLn (separador "========================================")

--------------------------------------------------------------------------------
-- Nombre: pedirOpcionUsuario
-- Entrada: ninguna
-- Salida: opción ingresada por el usuario (String)
-- Restricciones: lectura por consola
--------------------------------------------------------------------------------
pedirOpcionUsuario :: IO String
pedirOpcionUsuario = do
    putStrLn ""
    putStr (inputMsg "Seleccione una opción: ")
    hFlush stdout
    opcion <- getLine
    putStrLn ""
    return opcion

--------------------------------------------------------------------------------
-- ANÁLISIS TEMPORAL UI
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: imprimirLineaUI
-- Entrada: texto
-- Salida: imprime línea en consola
-- Restricciones: ninguna
--------------------------------------------------------------------------------
imprimirLinea :: String -> IO ()
imprimirLinea = putStrLn

--------------------------------------------------------------------------------
-- Nombre: imprimirTituloUI
-- Entrada: texto del título
-- Salida: imprime título formateado
-- Restricciones: solo UI
--------------------------------------------------------------------------------
imprimirTitulo :: String -> IO ()
imprimirTitulo txt = do

    imprimirLinea ("\n" ++ titulo linea)
    imprimirLinea (titulo ("        " ++ txt))
    imprimirLinea (titulo linea)

    where

    linea = "========================================"

--------------------------------------------------------------------------------
-- Nombre: mostrarMesDiaUI
-- Entrada: mes, monto, día, cantidad de eventos
-- Salida: muestra mes con mayor monto y día más activo
-- Restricciones: datos ya calculados previamente
--------------------------------------------------------------------------------
mostrarMesDiaUI :: String -> Float -> String -> Int -> IO ()
mostrarMesDiaUI mes monto dia cantidad = do

    imprimirTitulo "MES CON MAYOR MONTO"

    imprimirLinea (texto (alinearTexto mes 18) ++ " | " ++ okMsg (formatearMonto monto))

    imprimirTitulo "DÍA MÁS ACTIVO"

    imprimirLinea (texto (alinearTexto dia 18) ++ " | " ++ okMsg (show cantidad ++ " eventos"))

--------------------------------------------------------------------------------
-- Nombre: mostrarEventosExtremosUI
-- Entrada: evento más antiguo y más reciente
-- Salida: imprime ambos eventos
-- Restricciones: eventos válidos
--------------------------------------------------------------------------------
mostrarExtremosUI :: Evento -> Evento -> IO ()
mostrarExtremosUI viejo nuevo = do

    imprimirTitulo "EVENTOS EXTREMOS"

    mostrarEvento "MÁS ANTIGUO" viejo

    mostrarEvento "MÁS RECIENTE" nuevo

--------------------------------------------------------------------------------
-- Nombre: mostrarEventoUI
-- Entrada: etiqueta y evento
-- Salida: imprime información del evento
-- Restricciones: formato válido del evento
--------------------------------------------------------------------------------
mostrarEvento :: String -> Evento -> IO ()
mostrarEvento tituloEvento evento = do
    imprimirLinea ("\n" ++ separador linea)
    imprimirLinea (subtitulo ("        " ++ tituloEvento))
    imprimirLinea (separador linea)

    imprimirLinea (texto ("ID    : " ++ show (idEvento evento)))
    imprimirLinea (texto ("Fecha : " ++ formatearFecha (timestamp evento)))
    where
    linea = "----------------------------------------"

--------------------------------------------------------------------------------
-- RESUMEN POR INTERVALO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Nombre: imprimirResumenPorIntervaloUI
-- Entrada: lista de grupos (inicio, fin, eventos)
-- Salida: resumen visual por rango de fechas
-- Restricciones: eventos agrupados previamente
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Nombre: imprimirGrupoUI
-- Entrada: rango de fechas y eventos
-- Salida: imprime resumen del grupo
-- Restricciones: lista no vacía opcional
--------------------------------------------------------------------------------
imprimirGrupo :: (Day, Day, [Evento]) -> IO ()
imprimirGrupo (inicioDay, finDay, grupo) = do
    let 
        inicio = formatearFecha (dayToInt inicioDay)

        fin    = formatearFecha (dayToInt finDay)

        rango  = inicio ++ " - " ++ fin

        cantidad = length grupo

        montoTotal = sum (map total grupo)

    imprimirLinea $

        texto (ajustarTexto rango 40) ++ " | " ++
        texto (ajustarNumero cantidad 8) ++ " | " ++
        okMsg (ajustarTexto (formatearMonto montoTotal) 18)

--------------------------------------------------------------------------------
-- Nombre: pedirIntervaloDias
-- Entrada: máximo de días permitidos
-- Salida: número entero válido dentro del rango
-- Restricciones:
--   - Solo acepta valores numéricos entre 1 y maxDias
--------------------------------------------------------------------------------
pedirIntervaloUI :: Int -> IO Int
pedirIntervaloUI maxDias = do
    putStrLn ""

    putStr (inputMsg ("Ingrese el intervalo en días (1 - " ++ show maxDias ++ "): "))
    hFlush stdout

    entradaUsuario <- getLine

    case reads entradaUsuario :: [(Int, String)] of

        -- Caso válido dentro del rango permitido
        [(diasIngresados, "")] | diasIngresados >= 1 && diasIngresados <= maxDias ->
            return diasIngresados

        -- Número válido pero fuera de rango
        [(diasIngresados, "")] -> do
            imprimirLinea (errorMsg "Número fuera del rango permitido.")
            pedirIntervaloUI maxDias

        _ -> do
            imprimirLinea (errorMsg "Entrada inválida. Debe ser un número.")
            pedirIntervaloUI maxDias

--------------------------------------------------------------------------------
-- =========================
-- REPORTES UI
-- =========================
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Nombre: reporteImpuestos
-- Entrada: eventos: lista de eventos del sistema
-- Salida: imprime un resumen de eventos con cálculo de impuestos
-- Restricciones:
--   - Solo cuenta eventos de tipo Compra
--   - Depende de actualización previa de totales
--------------------------------------------------------------------------------
reporteImpuestos :: [Evento] -> IO ()
reporteImpuestos eventos = do

    let 
        eventosConTotal = actualizarTotales eventos

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

--------------------------------------------------------------------------------
-- Nombre: reporteEtiquetas
-- Entrada: eventos: lista de eventos del sistema
-- Salida: imprime análisis de eventos con etiqueta de alto valor
-- Restricciones:
--   - Usa promedios por categoría
--   - Depende de etiquetado previo de eventos
--------------------------------------------------------------------------------
reporteEtiquetas :: [Evento] -> IO ()
reporteEtiquetas eventos = do

    let 
        eventosEtiqueta = etiquetarAltoValor eventos

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
        ++ "Sobre promedio"))

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

--------------------------------------------------------------------------------
-- Nombre: reporteCompleto
-- Entrada: eventos: lista de eventos del sistema
-- Salida: muestra resumen general de métricas del sistema
-- Restricciones:
--   - Requiere eventos actualizados con impuestos y etiquetas
--------------------------------------------------------------------------------
reporteCompleto :: [Evento] -> IO ()
reporteCompleto eventos = do

    let 
        eventosFinales = actualizarTotales eventos

        totalImp = length (filter (\e -> categoria e == Compra && impuesto e > 0) eventosFinales)
        
        totalEti = length (filter etiqueta eventosFinales)

    putStrLn (titulo "\n══════════════════════════════════════")
    putStrLn (subtitulo "        REPORTE COMPLETO")
    putStrLn (titulo "══════════════════════════════════════")

    putStrLn (texto ("Eventos         : " ++ show (length eventosFinales)))
    putStrLn (okMsg ("Impuestos       : " ++ show totalImp))
    putStrLn (okMsg ("Sobre promedio  : " ++ show totalEti))

    putStrLn (titulo "══════════════════════════════════════")


--------------------------------------------------------------------------------
-- MENSAJES DEL SISTEMA
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Nombre: mostrarError
-- Entrada: ninguna
-- Salida: imprime mensaje de error por opción inválida
-- Restricciones:
--   - Solo se usa cuando el usuario ingresa una opción incorrecta
--------------------------------------------------------------------------------
mostrarError :: IO ()
mostrarError = do
    putStrLn (titulo "==================================")
    putStrLn (errorMsg "     OPCIÓN INVÁLIDA")
    putStrLn (titulo "==================================")
    putStrLn ""


--------------------------------------------------------------------------------
-- Nombre: mostrarSalidaSistema
-- Entrada: ninguna
-- Salida: imprime mensaje de salida del sistema
-- Restricciones:
--   - Se usa al cerrar el programa
--------------------------------------------------------------------------------
mostrarSalidaSistema :: IO ()
mostrarSalidaSistema = do
    putStrLn (titulo "==================================")
    putStrLn (okMsg "     SALIENDO DEL SISTEMA")
    putStrLn (titulo "==================================")
    putStrLn ""


--------------------------------------------------------------------------------
-- Nombre: mostrarResumenActualizacion
-- Entrada: cantidad de eventos agregados
-- Salida: muestra resumen de actualización del sistema
-- Restricciones:
--   - Solo muestra información, no modifica datos
--------------------------------------------------------------------------------
mostrarResumenActualizacion :: Int -> IO ()
mostrarResumenActualizacion cantidadEventosAgregados = do

    putStrLn (titulo "====================================")
    putStrLn (subtitulo "        ACTUALIZACIÓN SISTEMA")
    putStrLn (titulo "====================================")
    putStrLn (okMsg ("Eventos agregados: " ++ show cantidadEventosAgregados))
    putStrLn (titulo "====================================")
    putStrLn ""


--------------------------------------------------------------------------------
-- Nombre: mostrarEstadistica
-- Entrada: lista de eventos y estructura de estadística
-- Salida: imprime análisis completo de estadísticas del sistema
-- Restricciones:
--   - Depende de funciones de análisis externas
--   - Requiere lista de eventos válida
--------------------------------------------------------------------------------
mostrarEstadistica :: [Evento] -> Estadistica -> IO ()
mostrarEstadistica eventos e = do

    putStrLn (titulo "\n╔════════════════════════════════════╗")
    putStrLn (titulo "           ESTADÍSTICAS            ")
    putStrLn (titulo "╚════════════════════════════════════╝")

    putStrLn (okMsg ("ID: " ++ show (estId e)))
    putStrLn (infoMsg ("Fecha consulta: " ++ formatearFecha (fechaConsulta e)))

    putStrLn (subtitulo "\nResumen de categorías:")
    putStrLn (texto (eventosPorCategoria e))

    let maxE = eventoMaxCSV eventos

        minE = eMinimoSinDev eventos

        dia  = calcularDiaMasActivo eventos

    putStrLn (amarillo ++ "\n── MÁXIMO ──────────────────────────────" ++ reset)
    putStrLn ("│ ID: " ++ formatearLinea maxE)
    putStrLn (amarillo ++ "───────────────────────────────────────" ++ reset)

    putStrLn (rojo ++ "\n── MÍNIMO ──────────────────────────────" ++ reset)
    putStrLn ("│ ID: " ++ formatearLinea minE)
    putStrLn (rojo ++ "───────────────────────────────────────" ++ reset)

    putStrLn (magenta ++ "\n── DÍA MÁS ACTIVO ──────────────────────" ++ reset)
    putStrLn ("│ " ++ safeFecha dia)
    putStrLn (magenta ++ "───────────────────────────────────────" ++ reset)

    putStrLn (titulo "╚════════════════════════════════════╝")

--------------------------------------------------------------------------------
-- Nombre: formatearLinea
-- Entrada: cadena con información de evento
-- Salida: texto formateado con ID y monto
-- Restricciones:
--   - El formato esperado es "id,monto"
--------------------------------------------------------------------------------
formatearLinea :: String -> String
formatearLinea s =
    case break (== ',') s of
        (idv, _:resto) ->
            case readMaybe resto :: Maybe Float of
                Just n ->
                    " " ++ idv ++ "  |  Monto: " ++ formatearMonto n
                Nothing ->
                    " " ++ idv ++ "  |  Monto: 0.00"
        _ -> s

--------------------------------------------------------------------------------
-- Nombre: safeFecha
-- Entrada: fecha en formato string
-- Salida: fecha formateada de forma segura YYYY-MM-DD
-- Restricciones:
--   - Si el formato es inválido, devuelve el texto original
--------------------------------------------------------------------------------
safeFecha :: String -> String
safeFecha s =
    let clean = quitarGuiones s
    in if length clean == 8
        then take 4 clean ++ "-" ++ take 2 (drop 4 clean) ++ "-" ++ drop 6 clean
        else clean

--------------------------------------------------------------------------------
-- Nombre: imprimirEncabezado
-- Entrada: fecha inicio y fecha fin
-- Salida: muestra encabezado de búsqueda por rango
-- Restricciones:
--   - Solo presentación visual
--------------------------------------------------------------------------------
imprimirEncabezado :: String -> String -> IO ()
imprimirEncabezado fechaInicio fechaFin = do

    putStrLn (titulo "╔══════════════════════════════════════╗")
    putStrLn (titulo "   BÚSQUEDA POR RANGO DE FECHAS       ")
    putStrLn (titulo "╚══════════════════════════════════════╝")

    putStrLn (infoMsg (" Desde: " ++ fechaInicio))
    putStrLn (infoMsg (" Hasta: " ++ fechaFin))

--------------------------------------------------------------------------------
-- Nombre: imprimirTabla
-- Entrada: ninguna
-- Salida: imprime encabezado de tabla de eventos
-- Restricciones:
--   - Solo formato visual
--------------------------------------------------------------------------------
imprimirTabla :: IO ()
imprimirTabla = do

    putStrLn (separador "════════════════════════════════════════════════════════════════════")

    putStrLn (
        okMsg      (col "FECHA" 12) ++ "|" ++
        okMsg      (col "ID" 8) ++ "|" ++
        titulo     (col "CATEGORIA" 14) ++ "|" ++
        texto      (col "VALOR" 14) ++ "|" ++
        warningMsg (col "CANT" 6) ++ "|" ++
        errorMsg   (col "TOTAL" 14))

    putStrLn (separador "════════════════════════════════════════════════════════════════════")


--------------------------------------------------------------------------------
-- Nombre: imprimirFila
-- Entrada: evento
-- Salida: imprime una fila de la tabla de eventos
-- Restricciones:
--   - Usa cálculos externos para subtotal
--------------------------------------------------------------------------------
imprimirFila :: Evento -> IO ()
imprimirFila evento = do

    let subtotalEvento = calcularSubtotal (valor evento) (cantidad evento)
        totalVisual = formatearTotal evento subtotalEvento

    putStrLn (
        texto      (col (formatearFecha (timestamp evento)) 12) ++ "|" ++
        infoMsg    (col (show (idEvento evento)) 8) ++ "|" ++
        titulo     (col (show (categoria evento)) 14) ++ "|" ++
        texto      (col (formatearMonto (valor evento)) 14) ++ "|" ++
        warningMsg (col (show (cantidad evento)) 6) ++ "|" ++
        totalVisual)

    putStrLn (separador "--------------------------------------------------------------------")


--------------------------------------------------------------------------------
-- Nombre: imprimirResumen
-- Entrada: lista de eventos
-- Salida: muestra resumen total de eventos
-- Restricciones:
--   - Requiere lista válida de eventos
--------------------------------------------------------------------------------
imprimirResumen :: [Evento] -> IO ()
imprimirResumen eventos = do

    putStrLn (separador "════════════════════════════════════════════════════════════════════")

    putStrLn (okMsg (" Total encontrados: " ++ show (length eventos)))
    putStrLn (titulo (" Monto total: " ++ formatearMonto (sum (map total eventos))))

    putStrLn (separador "════════════════════════════════════════════════════════════════════")

--------------------------------------------------------------------------------
-- Nombre: mostrarPromedioCategoria
-- Entrada: categoría y promedio
-- Salida: imprime promedio por categoría
-- Restricciones:
--   - Solo presentación
--------------------------------------------------------------------------------
mostrarPromedioCategoria :: Categoria -> Float -> IO ()
mostrarPromedioCategoria categoria promedio =
    putStrLn (texto (ajustarTexto (show categoria) 16) ++ " | " ++ okMsg (formatearMonto promedio))