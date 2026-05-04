module Main where

import UI.Menus
import UI.Interfaz
import Core.Motor
import Services.GeneradorEventos
import Utils.CSV

--------------------------------------------------------------------------------
-- Nombre: main
--
-- Entrada: Ninguna (IO inicial del programa)
--
-- Salida:  IO () -> ejecuta el sistema completo con menú interactivo
--
-- Restricciones:
--   - Debe existir el archivo "data/eventos.csv" 
--   - El sistema depende de que los módulos UI, Core y CSV estén correctos
--   - Se asume entorno válido de ejecución 
--------------------------------------------------------------------------------
main :: IO ()
main = do

    let ruta = "data/eventos.csv"

    mostrarBienvenida

    eventosExistentes <- leerEventosSeguro ruta

    cicloMenu ruta eventosExistentes