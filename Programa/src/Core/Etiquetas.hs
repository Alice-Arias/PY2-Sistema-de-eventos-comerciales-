module Core.Etiquetas where

import Types.Evento
import Types.Categoria
import Core.Promedios


etiquetarAltoValor :: [Evento] -> [Evento]
etiquetarAltoValor listaEventos =
    let promedios = calcularPromedios listaEventos
    in map (etiquetarEvento promedios) listaEventos


etiquetarEvento :: [(Categoria, Float)] -> Evento -> Evento
etiquetarEvento promedios evento = evento { etiqueta = esAltoValor promedios evento }

esAltoValor :: [(Categoria, Float)] -> Evento -> Bool
esAltoValor promedios evento =
    case obtenerPromedioCategoria promedios (categoria evento) of
        Nothing       -> False
        Just promedio -> valor evento > promedio

obtenerPromedioCategoria :: [(Categoria, Float)] -> Categoria -> Maybe Float
obtenerPromedioCategoria promedios categoriaEvento = lookup categoriaEvento promedios