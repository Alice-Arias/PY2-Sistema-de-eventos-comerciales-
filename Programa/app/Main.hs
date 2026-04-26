module Main where

import Utils.CSV
import Services.GeneradorEventos

main :: IO ()
main = do
    let ruta = "data/eventos.csv"

    eventosExistentes <- leerEventosSeguro ruta

    nuevosEventos <- generarEventos eventosExistentes

    mapM_ (agregarEventoSeguro ruta) nuevosEventos

    putStrLn "Eventos generados y guardados"