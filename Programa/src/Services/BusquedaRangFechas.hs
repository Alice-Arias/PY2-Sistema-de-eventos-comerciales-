module Services.BusquedaRangFechas where

import Types.Modelos
import Types.Fecha
import Utils.Colores
import Utils.Formato
import Data.List (sortBy)
import Utils.Calculos
import UI.Interfaz

--------------------------------------------------------------------------------
-- Nombre: buscarPorRangoFechas
-- Entrada:
--   lista de eventos del sistema
--   fecha de inicio en texto (formato dd-mm-yyyy)
--   fecha final en texto (formato dd-mm-yyyy)
-- Salida:
--   Muestra en pantalla los eventos que están dentro del rango de fechas
-- Restricciones:
--   - Las fechas deben estar en formato válido
--------------------------------------------------------------------------------
buscarPorRangoFechas :: [Evento] -> String -> String -> IO ()
buscarPorRangoFechas eventos textoFechaInicio textoFechaFin =

    case (parseFecha textoFechaInicio, parseFecha textoFechaFin) of

        (Just fechaInicio, Just fechaFin) -> procesarBusquedaPorFechas eventos fechaInicio fechaFin textoFechaInicio textoFechaFin

        _ ->
            putStrLn (errorMsg "Fechas inválidas. Usa formato dd-mm-yyyy")


--------------------------------------------------------------------------------
-- Nombre: procesarBusquedaPorFechas
-- Entrada:
--   lista de eventos
--   fecha de inicio en número
--   fecha final en número
--   fechas en texto (solo para mostrar)
-- Salida:
--   imprime los eventos dentro del rango
-- Restricciones:
--   - el rango debe ser válido
--------------------------------------------------------------------------------
procesarBusquedaPorFechas :: [Evento] -> Int -> Int -> String -> String -> IO ()
procesarBusquedaPorFechas eventos fechaInicio fechaFin textoInicio textoFin = do

    let 
        (fechaInicioOrdenada, fechaFinOrdenada) = ordenarFechas fechaInicio fechaFin

        eventosDentroDelRango = filtrarEventosEnRango eventos fechaInicioOrdenada fechaFinOrdenada

        eventosOrdenados = ordenarEventosPorFecha eventosDentroDelRango

    imprimirEncabezado textoInicio textoFin
    imprimirTabla

    if null eventosOrdenados
    then putStrLn (errorMsg "No hay eventos en este rango de fechas.")
    else mapM_ imprimirFila eventosOrdenados

    imprimirResumen eventosOrdenados