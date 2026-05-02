module Services.Analisis.MontoTotal where

import Types.Evento
import Types.Categoria (Categoria(..))
import Utils.Formato (formatearMonto)
import Utils.Colores

montoTotal :: [Evento] -> IO ()
montoTotal eventos = do

    let totalRegistrado = sum (map total eventos)

        totalCorregido =  sum (map calcularTotalCorrecto eventos)

        diferencia = totalCorregido - totalRegistrado

        compras = filter (\e -> categoria e == Compra) eventos

        comprasSinImpuesto =  filter (\e -> impuesto e == 0) compras

        cantidadSinImpuesto = length comprasSinImpuesto

    putStrLn (titulo "\n========================================")
    putStrLn (titulo "            MONTO TOTAL")
    putStrLn (titulo "========================================")
    putStrLn (subtitulo " RESUMEN GENERAL")
    putStrLn (separador "----------------------------------------")
    putStrLn (texto "  Registrado en sistema : " ++ okMsg (formatearMonto totalRegistrado))
    putStrLn (texto "  Diferencia no aplicada : " ++ okMsg (formatearMonto diferencia))
    putStrLn (separador "----------------------------------------")
    putStrLn (subtitulo " ESTADO DE IMPUESTOS")
    putStrLn (separador "----------------------------------------")

    if cantidadSinImpuesto > 0
        then do
            putStrLn (warningMsg ("  Compras sin impuesto : " ++ show cantidadSinImpuesto))
            putStrLn (errorMsg "  Acción requerida     : Aplicar impuesto")
        else do
            putStrLn (okMsg "  Impuestos correctos")
            putStrLn (okMsg "  Estado del sistema   : Consistente")

    putStrLn (separador "----------------------------------------")

    putStrLn (subtitulo " EVENTOS SIN IMPUESTO")
    putStrLn (separador "----------------------------------------")

    if null comprasSinImpuesto
        then putStrLn (texto "  No hay eventos pendientes.")
        else mapM_ mostrarEvento comprasSinImpuesto

    putStrLn (titulo "========================================")
    putStrLn (subtitulo " TOTAL FINAL DEL SISTEMA ")
    putStrLn (titulo "========================================")

    putStrLn (okMsg ("  " ++ formatearMonto totalCorregido))

    putStrLn (titulo "========================================")


calcularTotalCorrecto :: Evento -> Float
calcularTotalCorrecto e =
    let subtotal = valor e * fromIntegral (cantidad e)
        impuesto = 
            if categoria e == Compra
            then subtotal * 0.13
            else 0
    in subtotal + impuesto

mostrarEvento :: Evento -> IO ()
mostrarEvento e = do

    let subtotal = valor e * fromIntegral (cantidad e)
        impuesto = subtotal * 0.13
        total = subtotal + impuesto

    putStrLn (infoMsg ("   Evento ID       : " ++ show (idEvento e)))
    putStrLn (texto "   Valor unitario  : " ++ okMsg (formatearMonto (valor e)))
    putStrLn (texto "   Cantidad        : " ++ cyan ++ show (cantidad e) ++ reset)

    putStrLn (texto "   Subtotal        : " ++ okMsg (formatearMonto subtotal))
    putStrLn (texto "   Impuesto        : " ++ warningMsg (formatearMonto impuesto))
    putStrLn (texto "   Total correcto  : " ++ okMsg (formatearMonto total))

    putStrLn (separador "----------------------------------------")
