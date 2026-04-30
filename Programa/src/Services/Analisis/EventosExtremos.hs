module Services.Analisis.EventosExtremos where

import Types.Evento
import Data.List (sortOn)

-- Devuelve (evento más antiguo, evento más reciente)
eventosExtremos :: [Evento] -> (Evento, Evento)
eventosExtremos [] = error "No hay eventos para analizar"

eventosExtremos eventos =
    let eventosOrdenadosPorFecha = sortOn timestamp eventos

        eventoMasAntiguo  = head eventosOrdenadosPorFecha
        eventoMasReciente  = last eventosOrdenadosPorFecha

    in (eventoMasAntiguo, eventoMasReciente)