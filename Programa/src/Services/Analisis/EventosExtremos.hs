module Services.Analisis.EventosExtremos where

import Types.Evento
import Data.List (sortOn)


ordenarEventosPorFecha :: [Evento] -> [Evento]
ordenarEventosPorFecha = sortOn timestamp

obtenerPrimero :: [Evento] -> Evento
obtenerPrimero = head

obtenerUltimo :: [Evento] -> Evento
obtenerUltimo = last

eventosExtremos :: [Evento] -> (Evento, Evento)
eventosExtremos [] = error "No hay eventos para analizar"

eventosExtremos eventos =
    let
        eventosOrdenados = ordenarEventosPorFecha eventos

        eventoMasAntiguo = obtenerPrimero eventosOrdenados
        eventoMasReciente = obtenerUltimo eventosOrdenados

    in
        (eventoMasAntiguo, eventoMasReciente)