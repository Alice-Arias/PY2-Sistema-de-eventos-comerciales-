module Services.BusquedaRangFechas where

import Types.Modelos
import Types.Fecha
import Utils.Colores
import Utils.Formato
import Data.List 
import Utils.Calculos
import UI.Interfaz

--------------------------------------------------------------------------------
-- Nombre: buscarPorRangoFechas
--
-- Objetivo: valida las fechas ingresadas y ejecuta la búsqueda de eventos
--           dentro de un rango de fechas
--
-- Entradas:
--   - lista de eventos del sistema
--   - fecha inicial en formato texto (dd-mm-yyyy)
--   - fecha final en formato texto (dd-mm-yyyy)
--
-- Salida: IO () con resultados impresos en pantalla
--
-- Restricciones:
--   - las fechas deben cumplir el formato dd-mm-yyyy
--   - si el formato es inválido, se muestra un mensaje de error
--------------------------------------------------------------------------------

buscarPorRangoFechas :: [Evento] -> String -> String -> IO ()
buscarPorRangoFechas eventos textoInicio textoFin =

    case (parseFecha textoInicio, parseFecha textoFin) of

        (Just inicio, Just fin) ->
            if fin < inicio

                then putStrLn (errorMsg "La fecha de fin no puede ser menor que la de inicio")

                else do
                    let filtrados = filtrarEventosEnRango eventos inicio fin

                    -- aquí muestras los resultados
                    print filtrados

        _ ->
            putStrLn (errorMsg "Fechas inválidas. Usa formato dd-mm-yyyy")


--------------------------------------------------------------------------------
-- Nombre: procesarBusquedaPorFechas
--
-- Objetivo: filtra y muestra los eventos que están dentro de un rango de fechas
--
-- Entradas:
--   - lista de eventos
--   - fecha inicial (entero)
--   - fecha final (entero)
--   - fecha inicial en texto (solo para interfaz)
--   - fecha final en texto (solo para interfaz)
--
-- Salida: IO () con tabla de eventos filtrados
--
-- Restricciones:
--   - el rango de fechas debe ser válido (inicio <= fin)
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