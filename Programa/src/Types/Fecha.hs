module Types.Fecha where

import Data.Time
import Text.Printf

-- Convierte YYYYMMDD (Int) a Day
intToDay :: Int -> Day
intToDay x =
    fromGregorian (fromIntegral anio) mes dia
  where
    anio = x `div` 10000
    mes  = (x `div` 100) `mod` 100
    dia  = x `mod` 100


-- Convierte Day a YYYYMMDD (por si lo necesitas después)
dayToInt :: Day -> Int
dayToInt d =
    read (printf "%04d%02d%02d" y m da)
  where
    (y, m, da) = toGregorian d