module Utils.Formato where

import Numeric (showFFloat)
import Data.List (intercalate)

formatearMonto :: Float -> String
formatearMonto n =
    "CRC " ++ formatearConComas (showFFloat (Just 2) n "")
    
formatearConComas :: String -> String
formatearConComas str =
    let (entero, decimal) = span (/='.') str
        enteroRev = reverse entero
        grupos = agrupar3 enteroRev
    in reverse (intercalate "," grupos) ++ decimal

agrupar3 :: String -> [String]
agrupar3 [] = []
agrupar3 xs = take 3 xs : agrupar3 (drop 3 xs)

