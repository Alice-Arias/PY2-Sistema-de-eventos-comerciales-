module Services.Analisis.MontoTotal where

import Types.Modelos
import Utils.Formato
import Utils.Colores
import Utils.Calculos

--------------------------------------------------------------------------------
-- Nombre: montoTotal
-- Entrada: lista de eventos del sistema (todas las acciones registradas)
-- Salida:
--   Muestra un resumen del dinero del sistema:
--   - dinero total registrado
--   - dinero perdido por devoluciones
--   - estado de impuestos
--   - lista de compras sin impuesto
--   - total final ya corregido
-- Restricciones:
--   - Puede funcionar con lista vacía (solo mostrará ceros o vacío)
--------------------------------------------------------------------------------
montoTotal :: [Evento] -> IO ()
montoTotal eventos = do

    let
        eventosDevoluciones = filter (\e -> categoria e == Devolucion) eventos

        eventosCompras = filter (\e -> categoria e == Compra) eventos

        dineroTotalSistema = sum (map total eventos)

        dineroPerdidoDevoluciones = sum (map total eventosDevoluciones)

        dineroFinalCorregido = dineroTotalSistema - dineroPerdidoDevoluciones

        comprasSinImpuesto = filter (\e -> impuesto e == 0) eventosCompras

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

    putStrLn (okMsg ("  " ++ formatearMonto dineroFinalCorregido))

    putStrLn (titulo "========================================")


--------------------------------------------------------------------------------
-- Nombre: calcularTotalCorrecto
-- Entrada: un evento individual
-- Salida: valor real del evento con impuesto incluido si aplica
-- Restricciones:
--   - Solo aplica impuesto si es una compra
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
-- Entrada: un evento individual
-- Salida: muestra en pantalla el detalle del evento
-- Restricciones:
--   - Solo muestra información, no modifica nada
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