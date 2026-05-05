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
import Utils.Colores (titulo, magenta2, subtitulo, opcion)

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
    putStrLn (titulo "\n════════════════════════════════════════")
    putStrLn (titulo "        SISTEMA DE EVENTOS")
    putStrLn (titulo "════════════════════════════════════════")
    putStrLn (subtitulo "        Bienvenido al sistema")
    putStrLn ""

--------------------------------------------------------------------------------
-- Nombre: mostrarMenuPrincipal
-- Entrada: ninguna
-- Salida: imprime opciones del menú principal
-- Restricciones: solo interfaz, no lógica de negocio
--------------------------------------------------------------------------------
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn (separador "\n════════════════════════════════════════")
    putStrLn (magenta2 "            MENÚ PRINCIPAL")
    putStrLn (separador "════════════════════════════════════════")

    putStrLn (opcion " 1. Transformación de eventos")
    putStrLn (opcion " 2. Análisis de datos")
    putStrLn (opcion " 3. Análisis temporal")
    putStrLn (opcion " 4. Búsqueda")
    putStrLn (opcion " 5. Estadísticas")
    putStrLn (opcion " 6. Salir")

    putStrLn (separador "════════════════════════════════════════")

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

    linea = "════════════════════════════════════════"

--------------------------------------------------------------------------------
-- Nombre: mostrarMesDiaUI
-- Entrada: mes, monto, día, cantidad de eventos
-- Salida: muestra mes con mayor monto y día más activo
-- Restricciones: datos ya calculados previamente
--------------------------------------------------------------------------------
mostrarMesDiaUI :: String -> Float -> String -> Int -> IO ()
mostrarMesDiaUI mes monto dia cantidad = do

    imprimirTitulo (magenta2 "MES CON MAYOR MONTO")

    imprimirLinea (texto (alinearTexto mes 18) ++ " | " ++ okMsg (formatearMonto monto))

    imprimirTitulo (magenta2 "DÍA MÁS ACTIVO")

    imprimirLinea (texto (alinearTexto dia 18) ++ " | " ++ okMsg (show cantidad ++ " eventos"))

--------------------------------------------------------------------------------
-- Nombre: mostrarEventosExtremosUI
-- Entrada: evento más antiguo y más reciente
-- Salida: imprime ambos eventos
-- Restricciones: eventos válidos
--------------------------------------------------------------------------------
mostrarExtremosUI :: Evento -> Evento -> IO ()
mostrarExtremosUI viejo nuevo = do

    imprimirTitulo (magenta2 "EVENTOS EXTREMOS")

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
    imprimirLinea (magenta2 ("        " ++ tituloEvento))
    imprimirLinea (separador linea)

    imprimirLinea ( cyan2 "ID    : " ++  texto (show (idEvento evento)))

    imprimirLinea (  cyan2 "Fecha : " ++  texto (formatearFecha (timestamp evento)))
    where
    
    linea = "════════════════════════════════════════"

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
    imprimirLinea (subtitulo "                 RESUMEN POR INTERVALO DE DÍAS")
    imprimirLinea (titulo linea)

    imprimirLinea $
        subtitulo (ajustarTexto "Rango de Fechas" 40) ++ " | " ++
        subtitulo (ajustarTexto "Cantidad" 8) ++ " | " ++
        subtitulo (ajustarTexto "Monto Total" 18)

    imprimirLinea (separador (replicate 70 '-'))

    mapM_ imprimirGrupo grupos
    where
    linea = "═════════════════════════════════════════════════════════════════════════════"

--------------------------------------------------------------------------------
-- Nombre: imprimirGrupoUI
-- Entrada: rango de fechas y eventos
-- Salida: imprime resumen del grupo
-- Restricciones: lista no vacía opcional
--------------------------------------------------------------------------------
imprimirGrupo :: (Day, Day, [Evento]) -> IO ()
imprimirGrupo (fechaInicio, fechaFin, eventosGrupo) = do
    let 
        fechaInicioTexto = formatearFecha (dayToInt fechaInicio)

        fechaFinTexto    = formatearFecha (dayToInt fechaFin)

        rangoFechas      = fechaInicioTexto ++ " - " ++ fechaFinTexto

        cantidadEventos  = length eventosGrupo

        montoTotalGrupo  = sum (map total eventosGrupo)

    imprimirLinea $

        opcion (ajustarTexto rangoFechas 40) ++ " | " ++
        texto (ajustarNumero cantidadEventos 8) ++ " | " ++
        okMsg (ajustarTexto (formatearMonto montoTotalGrupo) 18)

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

    let eventosEtiquetados = etiquetarAltoValor eventos

        categorias = nub (map categoria eventos)

        promedios = calcularPromedios eventos

        totalEventos = length eventos

        totalSobrePromedio =
            length (filter etiqueta eventosEtiquetados)

    putStrLn (titulo "\n════════════════════════════════════════")
    putStrLn (subtitulo "        REPORTE DE ETIQUETAS")
    putStrLn (titulo "════════════════════════════════════════")

    putStrLn (texto ("Total eventos      : " ++ show totalEventos))
    putStrLn (okMsg ("Sobre promedio     : " ++ show totalSobrePromedio))
    putStrLn ""

    putStrLn (colorBold magenta $
        ajustarTexto "Categoría" 15 ++ " | "
        ++ ajustarTexto "Total" 7 ++ " | "
        ++ ajustarTexto "Promedio" 18 ++ " | "
        ++ "Sobre promedio")

    putStrLn (separador (replicate 65 '-'))

    mapM_ (mostrarCategoria eventosEtiquetados promedios) categorias

    putStrLn (titulo "════════════════════════════════════════")

mostrarCategoria :: [Evento] -> [(Categoria, Float)] -> Categoria -> IO ()
mostrarCategoria eventosEtiquetados promedios categoriaActual =

    let eventosCategoria =   filter (\e -> categoria e == categoriaActual) eventosEtiquetados

        totalCategoria =  length eventosCategoria

        sobrePromedio =   length (filter etiqueta eventosCategoria)

        promedio =   obtenerPromedio categoriaActual promedios

    in putStrLn $
        ajustarTexto (show categoriaActual) 15 ++ " | "
        ++ inputMsg (ajustarNumero totalCategoria 7) ++ " | "
        ++ ajustarTexto (formatearMonto promedio) 18 ++ " | "
        ++ inputMsg (ajustarNumero sobrePromedio 6)

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
        eventosConTotales = actualizarTotales eventos

        cantidadComprasConImpuesto = length (filter (\evento -> categoria evento == Compra && impuesto evento > 0) eventosConTotales)

        cantidadEventosConEtiqueta =  length (filter etiqueta eventosConTotales)

    putStrLn (titulo "\n══════════════════════════════════════")
    putStrLn (subtitulo "        REPORTE COMPLETO")
    putStrLn (titulo "══════════════════════════════════════")

    putStrLn (texto ("Eventos         : " ++ show (length eventosConTotales)))
    putStrLn (okMsg ("Impuestos       : " ++ show cantidadComprasConImpuesto))
    putStrLn (okMsg ("Sobre promedio  : " ++ show cantidadEventosConEtiqueta))

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
    putStrLn (titulo "══════════════════════════════════════")
    putStrLn (errorMsg "     OPCIÓN INVÁLIDA")
    putStrLn (titulo "══════════════════════════════════════")
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
    putStrLn (titulo "══════════════════════════════════════")
    putStrLn (okMsg "     SALIENDO DEL SISTEMA")
    putStrLn (titulo "══════════════════════════════════════")
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

    putStrLn (titulo "══════════════════════════════════════")
    putStrLn (subtitulo "        ACTUALIZACIÓN SISTEMA")
    putStrLn (titulo "══════════════════════════════════════")
    putStrLn (okMsg ("Eventos agregados: " ++ show cantidadEventosAgregados))
    putStrLn (titulo "══════════════════════════════════════")
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
mostrarEstadistica listaEventos estadistica = do

    putStrLn (titulo "\n╔════════════════════════════════════╗")
    putStrLn (magenta2 "           ESTADÍSTICAS            ")
    putStrLn (titulo "╚════════════════════════════════════╝")

    putStrLn (okMsg ("ID: " ++ show (estId estadistica)))
    putStrLn (opcion ("Fecha consulta: " ++ formatearFecha (fechaConsulta estadistica)))

    putStrLn (subtitulo "\nResumen de categorías:")
    putStrLn (texto (eventosPorCategoria estadistica))

    let maximoEvento = eventoMaxCSV listaEventos
        minimoEvento = eMinimoSinDev listaEventos
        diaMasActivo = calcularDiaMasActivo listaEventos

    putStrLn (amarillo ++ "\n── MÁXIMO ──────────────────────────────" ++ reset)
    putStrLn ("│ ID: " ++ formatearLinea maximoEvento)
    putStrLn (amarillo ++ "───────────────────────────────────────" ++ reset)

    putStrLn (rojo ++ "\n── MÍNIMO ──────────────────────────────" ++ reset)
    putStrLn ("│ ID: " ++ formatearLinea minimoEvento)
    putStrLn (rojo ++ "───────────────────────────────────────" ++ reset)

    putStrLn (magenta ++ "\n── DÍA MÁS ACTIVO ──────────────────────" ++ reset)
    putStrLn ("│     " ++ safeFecha diaMasActivo)
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
formatearLinea lineaCSV =
    case break (== ',') lineaCSV of
        (idEventoTexto, _:restoTexto) ->

            case readMaybe restoTexto :: Maybe Float of

                Just monto ->  " " ++ idEventoTexto ++ "  |  Monto: " ++ formatearMonto monto
                Nothing -> " " ++ idEventoTexto ++ "  |  Monto: 0.00"
        _ -> lineaCSV

--------------------------------------------------------------------------------
-- Nombre: safeFecha
-- Entrada: fecha en formato string
-- Salida: fecha formateada de forma segura YYYY-MM-DD
-- Restricciones:
--   - Si el formato es inválido, devuelve el texto original
--------------------------------------------------------------------------------
safeFecha :: String -> String
safeFecha textoFecha =
    let 
        fechaSinGuiones = quitarGuiones textoFecha
    in 
        if length fechaSinGuiones == 8
        then take 4 fechaSinGuiones ++ "-" ++ take 2 (drop 4 fechaSinGuiones) ++ "-" ++ drop 6 fechaSinGuiones
        else fechaSinGuiones
--------------------------------------------------------------------------------
-- Nombre: imprimirEncabezado
-- Entrada: fecha inicio y fecha fin
-- Salida: muestra encabezado de búsqueda por rango
-- Restricciones:
--   - Solo presentación visual
--------------------------------------------------------------------------------
imprimirEncabezado :: String -> String -> IO ()
imprimirEncabezado fechaInicio fechaFin = do

    putStrLn (titulo "\n╔══════════════════════════════════════╗")
    putStrLn (magenta2 "   BÚSQUEDA POR RANGO DE FECHAS       ")
    putStrLn (titulo "╚══════════════════════════════════════╝")

    putStrLn (opcion (" Desde: " ++ fechaInicio))
    putStrLn (opcion (" Hasta: " ++ fechaFin))

--------------------------------------------------------------------------------
-- Nombre: imprimirTabla
-- Entrada: ninguna
-- Salida: imprime encabezado de tabla de eventos
-- Restricciones:
--   - Solo formato visual
--------------------------------------------------------------------------------
imprimirTabla :: IO ()
imprimirTabla = do

    putStrLn (separador "══════════════════════════════════════════════════════════════════════════════════════")

    putStrLn (
        okMsg      (col "FECHA" 18) ++ "|" ++
        okMsg      (col "ID" 8) ++ "|" ++
        titulo     (col "CATEGORIA" 16) ++ "|" ++
        texto      (col "VALOR" 14) ++ "|" ++
        warningMsg (col "CANT" 6) ++ "|" ++
        errorMsg   (col "TOTAL" 14))

    putStrLn (separador "══════════════════════════════════════════════════════════════════════════════════════")


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

        categoriaColor =
            case categoria evento of
                Devolucion -> warningMsg
                Compra     -> okMsg
                _          -> titulo

    putStrLn (
        texto        (col (formatearFecha (timestamp evento)) 18) ++ "|" ++
        infoMsg      (col (show (idEvento evento)) 8) ++ "|" ++
        categoriaColor (col (show (categoria evento)) 16) ++ "|" ++
        texto        (col (formatearMonto (valor evento)) 14) ++ "|" ++
        warningMsg   (col (show (cantidad evento)) 6) ++ "|" ++
        totalVisual)

    putStrLn (separador "══════════════════════════════════════════════════════════════════════════════════════")
--------------------------------------------------------------------------------
-- Nombre: imprimirResumen
-- Entrada: lista de eventos
-- Salida: muestra resumen total de eventos
-- Restricciones:
--   - Requiere lista válida de eventos
--------------------------------------------------------------------------------
imprimirResumen :: [Evento] -> IO ()
imprimirResumen eventos = do

    let eventosActualizados = actualizarTotales eventos

        totalesPorCategoria = construirTotalesPorCategoria eventosActualizados

        montoFinal = sum (map totalAjustado eventosActualizados)

    putStrLn (separador "══════════════════════════════════════════════════════════════════════════════════════")

    putStrLn (okMsg (" Total encontrados: " ++ show (length eventos)))

    putStrLn ""

    mapM_ (\(categoria, total) ->
        putStrLn (titulo (show categoria ++ " : " ++ formatearMonto total))
        ) totalesPorCategoria

    putStrLn ""

    putStrLn (magenta2 (" Monto final de ganancias : " ++ formatearMonto montoFinal))

    putStrLn (separador "══════════════════════════════════════════════════════════════════════════════════════")

--------------------------------------------------------------------------------
-- Nombre: mostrarPromedioCategoria
-- Entrada: categoría y promedio
-- Salida: imprime promedio por categoría
-- Restricciones:
--   - Solo presentación
--------------------------------------------------------------------------------
mostrarPromedioCategoria :: Categoria -> Float -> IO ()
mostrarPromedioCategoria categoria promedio =
    let nombreCategoria = ajustarTexto (show categoria) 16

        promedioFormateado =  formatearMonto promedio

        linea =
            texto nombreCategoria
            ++ " | "
            ++ okMsg promedioFormateado

    in putStrLn linea