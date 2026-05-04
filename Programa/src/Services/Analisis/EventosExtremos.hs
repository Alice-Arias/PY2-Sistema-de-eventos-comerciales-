module Services.Analisis.EventosExtremos where

import Types.Modelos
import Data.List

--------------------------------------------------------------------------------
-- Nombre: ordenarEventosPorFecha
--
-- Objetivo: ordena los eventos desde el más antiguo al más reciente según su timestamp
--
-- Entradas: lista de eventos
--
-- Salida: lista de eventos ordenados cronológicamente
--
-- Restricciones:
--   - cada evento debe tener un timestamp válido
--------------------------------------------------------------------------------
ordenarEventosPorFecha :: [Evento] -> [Evento]
ordenarEventosPorFecha = sortOn timestamp


--------------------------------------------------------------------------------
-- Nombre: obtenerPrimerEvento
--
-- Objetivo: obtiene el evento más antiguo de una lista ordenada
--
-- Entradas: lista de eventos ordenados por fecha
--
-- Salida: evento más antiguo
--
-- Restricciones:
--   - la lista no puede estar vacía
--------------------------------------------------------------------------------
obtenerPrimerEvento :: [Evento] -> Evento
obtenerPrimerEvento = head


--------------------------------------------------------------------------------
-- Nombre: obtenerUltimoEvento
--
-- Objetivo: obtiene el evento más reciente de una lista ordenada
--
-- Entradas: lista de eventos ordenados por fecha
--
-- Salida: evento más reciente
--
-- Restricciones:
--   - la lista no puede estar vacía
--------------------------------------------------------------------------------
obtenerUltimoEvento :: [Evento] -> Evento
obtenerUltimoEvento = last


--------------------------------------------------------------------------------
-- Nombre: eventosExtremos
--
-- Objetivo: obtiene el evento más antiguo y el más reciente del sistema
--
-- Entradas: lista de eventos
--
-- Salida: par (evento más antiguo, evento más reciente)
--
-- Restricciones:
--   - la lista no debe estar vacía
--   - si la lista está vacía, se lanza un error
--------------------------------------------------------------------------------
eventosExtremos :: [Evento] -> (Evento, Evento)
eventosExtremos [] = error "No hay eventos para analizar"

eventosExtremos eventos =

    let 
        eventosOrdenados = ordenarEventosPorFecha eventos
        eventoAntiguo = obtenerPrimerEvento eventosOrdenados
        eventoReciente = obtenerUltimoEvento eventosOrdenados
        
    in (eventoAntiguo, eventoReciente)