module Core.Etiquetas where

import Types.Evento
import Types.Categoria
import Core.Promedios

etiquetarAltoValor :: [Evento] -> [Evento]
etiquetarAltoValor eventos =
    let 
        promediosPorCategoria = calcularPromedios eventos
    in 
        map (etiquetarEvento promediosPorCategoria) eventos


etiquetarEvento :: [(Categoria, Float)] -> Evento -> Evento--maybe false es para evitar error de tipo, ya que lookup devuelve Maybe Float
etiquetarEvento promediosPorCategoria evento = evento { etiqueta = maybe False (compararValor (valor evento)) promedioDeCategoria }--compararValor es una función que compara el valor del evento con el promedio de su categoría, devuelve True si el valor es mayor que el promedio, False en caso contrario
    
    where
        promedioDeCategoria =lookup (categoria evento) promediosPorCategoria--lookup devuelve Maybe Float, por eso se usa maybe para manejar el caso de que no se encuentre la categoría

        compararValor valorEvento promedioCategoria = valorEvento > promedioCategoria