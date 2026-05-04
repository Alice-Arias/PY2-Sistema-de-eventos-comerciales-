module Services.Analisis.MontoTotal where

import Types.Modelos
import Utils.Formato
import Utils.Colores
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: montoTotal
--
-- Objetivo: muestra un resumen financiero general del sistema de eventos
--           incluyendo ingresos, devoluciones, estado de impuestos y totales
--
-- Entradas: lista de eventos del sistema
--
-- Salida: IO () con reporte detallado en pantalla
--
-- Restricciones:
--   - puede ejecutarse con lista vacía (mostrará valores en cero)
--------------------------------------------------------------------------------
montoTotal :: [Evento] -> IO ()
montoTotal eventos = do

    let
        eventosDevolucion = filter (\evento -> categoria evento == Devolucion) eventos

        eventosCompra = filter (\evento -> categoria evento == Compra) eventos

        dineroTotalSistema = sum (map total eventos)

        dineroPerdidoDevoluciones = sum (map total eventosDevolucion)

        dineroFinalSistema = dineroTotalSistema - dineroPerdidoDevoluciones

        comprasSinImpuesto = filter (\evento -> impuesto evento == 0) eventosCompra

        cantidadComprasSinImpuesto = length comprasSinImpuesto

    putStrLn (titulo "\n========================================")
    putStrLn (titulo "            MONTO TOTAL")
    putStrLn (titulo "========================================")

    putStrLn (subtitulo " RESUMEN GENERAL")
    putStrLn (separador "----------------------------------------")

    putStrLn (texto "  Dinero registrado   : " ++ okMsg (formatearMonto dineroTotalSistema))
    putStrLn (texto "  Devoluciones        : " ++ warningMsg (formatearMonto dineroPerdidoDevoluciones))

    putStrLn (separador "----------------------------------------")

    putStrLn (subtitulo " ESTADO DE IMPUESTOS")
    putStrLn (separador "----------------------------------------")

    if cantidadComprasSinImpuesto > 0
        then do
            putStrLn (warningMsg ("  Compras sin impuesto : " ++ show cantidadComprasSinImpuesto))
            putStrLn (errorMsg "  Falta aplicar impuestos")
        else do
            putStrLn (okMsg "  Todos los impuestos están correctos")
            putStrLn (okMsg "  Sistema consistente")

    putStrLn (separador "----------------------------------------")

    putStrLn (subtitulo " EVENTOS SIN IMPUESTO")
    putStrLn (separador "----------------------------------------")

    if null comprasSinImpuesto
        then putStrLn (texto "  No hay compras pendientes.")
        else mapM_ mostrarEvento comprasSinImpuesto

    putStrLn (titulo "========================================")
    putStrLn (subtitulo " TOTAL FINAL DEL SISTEMA ")
    putStrLn (titulo "========================================")

    putStrLn (okMsg ("  " ++ formatearMonto dineroFinalSistema))

    putStrLn (titulo "========================================")

--------------------------------------------------------------------------------
-- Nombre: calcularTotalCorrecto
--
-- Objetivo: calcula el total real de un evento incluyendo impuesto si aplica
--
-- Entradas: evento individual
--
-- Salida: valor final del evento
--
-- Restricciones:
--   - solo aplica impuesto a eventos de tipo Compra
--------------------------------------------------------------------------------
calcularTotalCorrecto :: Evento -> Float
calcularTotalCorrecto evento =
    let
        subtotal = valor evento * fromIntegral (cantidad evento)

        impuestoCalculado =
            if categoria evento == Compra
            then subtotal * 0.13
            else 0

    in subtotal + impuestoCalculado


--------------------------------------------------------------------------------
-- Nombre: mostrarEvento
--
-- Objetivo: muestra el detalle completo de un evento en pantalla
--
-- Entradas: evento individual
--
-- Salida: IO () con información formateada
--
-- Restricciones:
--   - no modifica datos, solo visualiza información
--------------------------------------------------------------------------------
mostrarEvento :: Evento -> IO ()
mostrarEvento evento = do

    let subtotal = valor evento * fromIntegral (cantidad evento)
        impuestoCalculado = subtotal * 0.13
        totalCalculado = subtotal + impuestoCalculado

    putStrLn (infoMsg ("   ID del evento    : " ++ show (idEvento evento)))
    putStrLn (texto "   Precio unitario  : " ++ okMsg (formatearMonto (valor evento)))
    putStrLn (texto "   Cantidad         : " ++ cyan ++ show (cantidad evento) ++ reset)

    putStrLn (texto "   Subtotal         : " ++ okMsg (formatearMonto subtotal))
    putStrLn (texto "   Impuesto         : " ++ warningMsg (formatearMonto impuestoCalculado))
    putStrLn (texto "   Total            : " ++ okMsg (formatearMonto totalCalculado))

    putStrLn (separador "----------------------------------------")