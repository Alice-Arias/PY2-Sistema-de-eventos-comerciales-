module Main where

import UI.Menu
import Core.Motor
import Services.GeneradorEventos
import Utils.CSV

main :: IO ()
main = do
    let ruta = "data/eventos.csv"

    mostrarBienvenida

    eventosExistentes <- leerEventosSeguro ruta
    cicloMenu ruta eventosExistentes