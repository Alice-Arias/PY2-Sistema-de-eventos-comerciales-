module Services.Analisis.EventosExtremos where

import Types.Modelos
import Data.List

--------------------------------------------------------------------------------
-- Nombre: ordenarEventosPorFecha
-- Entrada: lista de eventos del sistema
-- Salida: eventos ordenados desde el más antiguo al más reciente
-- Restricciones:
--   - Los eventos deben tener una fecha válida (timestamp)
--------------------------------------------------------------------------------
ordenarEventosPorFecha :: [Evento] -> [Evento]
ordenarEventosPorFecha = sortOn timestamp


--------------------------------------------------------------------------------
-- Nombre: obtenerPrimerEvento
-- Entrada: lista de eventos ordenados por fecha
-- Salida: evento más antiguo de la lista
-- Restricciones:
--   - La lista no puede estar vacía
--------------------------------------------------------------------------------
obtenerPrimerEvento :: [Evento] -> Evento
obtenerPrimerEvento = head


--------------------------------------------------------------------------------
-- Nombre: obtenerUltimoEvento
-- Entrada: lista de eventos ordenados por fecha
-- Salida: evento más reciente de la lista
-- Restricciones:
--   - La lista no puede estar vacía
--------------------------------------------------------------------------------
obtenerUltimoEvento :: [Evento] -> Evento
obtenerUltimoEvento = last


--------------------------------------------------------------------------------
-- Nombre: eventosExtremos
-- Entrada: lista de eventos del sistema
-- Salida:
--   un par de eventos:
--   (evento más antiguo, evento más reciente)
-- Restricciones:
--   - La lista no debe estar vacía
--   - Si está vacía, el sistema lanza un error
--------------------------------------------------------------------------------
eventosExtremos :: [Evento] -> (Evento, Evento)
eventosExtremos [] = error "No hay eventos para analizar"

eventosExtremos eventos =

    let 
        eventosOrdenados = ordenarEventosPorFecha eventos
        eventoAntiguo = obtenerPrimerEvento eventosOrdenados
        eventoReciente = obtenerUltimoEvento eventosOrdenados
        
    in (eventoAntiguo, eventoReciente)