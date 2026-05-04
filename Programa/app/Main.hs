module Main where

import UI.Menus
import UI.Interfaz
import Core.Motor
import Services.GeneradorEventos
import Utils.CSV
--------------------------------------------------------------------------------
-- Nombre: main
--
-- Objetivo: inicializa el sistema de eventos y lanza el menú interactivo
--
-- Entradas: ninguna (punto de entrada del programa)
--
-- Salida: IO () que ejecuta el sistema completo
--
-- Restricciones:
--   - Debe existir el archivo "data/eventos.csv"
--   - Los módulos UI, Core y CSV deben estar correctamente implementados
--   - El entorno de ejecución debe permitir lectura de archivos
--------------------------------------------------------------------------------
main :: IO ()
main = do

    let ruta = "data/eventos.csv"

    mostrarBienvenida

    eventosExistentes <- leerEventosSeguro ruta

    cicloMenu ruta eventosExistentes