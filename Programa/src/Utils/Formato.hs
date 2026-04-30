module Utils.Formato where

import Numeric (showFFloat)

-- =========================
-- FORMATO DE MONTO
-- =========================

formatearMonto :: Float -> String
formatearMonto monto =
    "CRC " ++ formatearDosDecimales monto


formatearDosDecimales :: Float -> String
formatearDosDecimales x =
    showFFloat (Just 2) x ""