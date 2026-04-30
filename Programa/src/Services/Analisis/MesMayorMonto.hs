module Services.Analisis.MesMayorMonto where

import Types.Evento
import Utils.Fecha (extraerMesAno)
import Data.List (nub, maximumBy)
import Data.Ord (comparing)


mesConMayorMonto :: [Evento] -> (String, Float)
mesConMayorMonto eventos =

    let

        -- extrae todos los meses únicos de los eventos
        mesesUnicos :: [String]
        mesesUnicos =
            nub (map (extraerMesAno . timestamp) eventos)


        -- calcula el total de un mes específico
        calcularTotalDelMes :: String -> Float
        calcularTotalDelMes mes =
            let eventosDelMes =
                    filter (\evento ->
                        extraerMesAno (timestamp evento) == mes
                    ) eventos

            in sum (map total eventosDelMes)


        -- crea pares (mes, total)
        resumenPorMes :: [(String, Float)]
        resumenPorMes =
            map (\mes ->
                (mes, calcularTotalDelMes mes)
            ) mesesUnicos


        -- obtiene el mes con mayor monto
        mesConMayorMontoFinal =
            maximumBy (comparing snd) resumenPorMes

    in
        mesConMayorMontoFinal