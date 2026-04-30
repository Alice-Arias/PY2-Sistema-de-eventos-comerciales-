module Services.Analisis.ResumenIntervalos where

import Types.Evento
import Utils.Fecha (extraerAnio, extraerMes, nombreMes)
import Data.List (nub, sortOn)

resumenIntervalos :: [Evento] -> [(String, Int, Float)]
resumenIntervalos eventos =

    let periodos =
            nub [(extraerAnio e, extraerMes e) | e <- eventos]

        ordenados =
            sortOn (\(a, m) -> (a, m)) periodos

        calcular (anio, mes) =
            let eventosFiltrados =
                    filter (\e -> extraerAnio e == anio && extraerMes e == mes) eventos

                cantidad = length eventosFiltrados
                monto = sum (map total eventosFiltrados)

            in (nombreMes mes ++ " " ++ show anio, cantidad, monto)

    in map calcular ordenados