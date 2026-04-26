-- Nombre: Utils.CSV
-- Descripcion: maneja la lectura, escritura y validación de eventos en archivos CSV.
-- Entradas: Usa el tipo Evento y rutas de archivo (FilePath).
-- Salidas: Funciones para validar eventos, convertir a CSV, agregar eventos a archivos y leer eventos desde archivos.
-- Validaciones: Se validan valores de Evento (valor, cantidad, impuesto), Maneja errores de archivo y datos inválidos.
module Utils.CSV where

import Data.Maybe (catMaybes)
import Types.Evento
import System.IO
import System.Directory (doesFileExist)
import Control.Exception


-- Nombre: validarEvento
-- Descripción: Verifica que los datos de un evento sean válidos.
-- Entradas:Un Evento.
-- Salidas:error o evento válido
-- Validaciones: valor > 0, cantidad > 0, impuesto >= 0.
validarEvento :: Evento -> Either String Evento
validarEvento evento
    | valor evento <= 0 = Left "Valor inválido"
    | cantidad evento <= 0 = Left "Cantidad inválida"
    | impuesto evento < 0 = Left "Impuesto inválido"
    | otherwise = Right evento


-- Nombre: eventoToCSV
-- Descripción: Convierte un Evento a texto en formato CSV.
-- Entradas:Un Evento.
-- Salidas: String en formato CSV.
-- Validaciones: No valida datos, solo convierte. Se asume que el evento ya es válido.
eventoToCSV :: Evento -> String
eventoToCSV e =
    show (idEvento e) ++ "," ++
    show (categoria e) ++ "," ++
    show (valor e) ++ "," ++
    show (timestamp e) ++ "," ++
    show (usuarioId e) ++ "," ++
    show (productoId e) ++ "," ++
    show (producto e) ++ "," ++
    show (cantidad e) ++ "," ++
    show (metodoPago e) ++ "," ++
    show (estado e) ++ "," ++
    show (impuesto e)


-- Nombre: existeEvento
-- Descripción: Verifica si un evento ya existe en una lista por su ID.
-- Entradas: Int (id a buscar), lista de Evento.
-- Salidas: Bool (True si existe).
-- Validaciones: Compara solo por idEvento, no valida otros campos ni consistencia.
existeEvento :: Int -> [Evento] -> Bool
existeEvento idBuscar = any (\e -> idEvento e == idBuscar)


-- Nombre: agregarEventoSeguro
-- Descripción: Agrega un evento a un archivo CSV si es válido y no está duplicado.
-- Entradas: Ruta del archivo (FilePath), Evento.
-- Salidas: IO () con mensaje en consola.
-- Validaciones: Verifica duplicados por ID, valida campos del evento, maneja errores de archivo. Solo agrega si el evento es válido y no existe.
agregarEventoSeguro :: FilePath -> Evento -> IO ()
agregarEventoSeguro ruta evento = do
    eventos <- leerEventosSeguro ruta
    
    if existeEvento (idEvento evento) eventos
        then putStrLn "Error: evento duplicado"
        else
            case validarEvento evento of
                Left errorMsg -> putStrLn ("Error: " ++ errorMsg)
                Right e -> do
                    resultado <- try (appendFile ruta (eventoToCSV e ++ "\n")) :: IO (Either IOException ())
                    case resultado of
                        Left _ -> putStrLn "Error al guardar el archivo"
                        Right _ -> putStrLn "Evento guardado correctamente"


-- Nombre: split
-- Descripción: Divide un texto usando un separador.
-- Entradas: Char , String.
-- Salidas: Lista de String.
-- Validaciones: No valida contenido, solo divide. Si el separador no está, devuelve la lista con el texto completo.
split :: Char -> String -> [String]
split _ [] = []
split separador texto =
    let (parte, resto) = break (== separador) texto
    in parte : case resto of
        [] -> []
        (_:restoSinSeparador) -> split separador restoSinSeparador

-- Nombre: csvToEventoSeguro
-- Descripción: Convierte una línea CSV a Evento si es válida.
-- Entradas: String (línea CSV).
-- Salidas: Maybe Evento.
-- Validaciones: Verifica que tenga 11 campos, que los tipos sean correctos. Devuelve Nothing si no es válido, Just Evento si es correcto. No valida valores específicos, solo formato y tipos.
csvToEventoSeguro :: String -> Maybe Evento
csvToEventoSeguro lineaCSV =
    let campos = split ',' lineaCSV
    in if length campos /= 11
        then Nothing
        else Just (Evento
            (read (head campos))           -- idEvento
            (read (campos !! 1))           -- categoria
            (read (campos !! 2))           -- valor
            (read (campos !! 3))           -- timestamp
            (read (campos !! 4))           -- usuarioId
            (read (campos !! 5))           -- productoId
            (campos !! 6)                  -- producto
            (read (campos !! 7))           -- cantidad
            (read (campos !! 8))           -- metodoPago
            (read (campos !! 9))           -- estado
            (read (campos !! 10))          -- impuesto
        )

-- NOMBRE: leerEventosSeguro
-- DESCRIPCIÓN: Lee un archivo CSV y devuelve una lista de eventos válidos.
-- ENTRADAS: Ruta del archivo
-- Salidas: IO [Evento]
-- Validaciones: Si el archivo no existe devuelve lista vacía. Ignora líneas inválidas. Solo devuelve eventos que se puedan convertir correctamente. No valida valores específicos, solo formato y tipos.
leerEventosSeguro :: FilePath -> IO [Evento]
leerEventosSeguro ruta = do
    existe <- doesFileExist ruta
    if not existe
        then return []
    else do
        contenido <- readFile ruta
        let lineas = lines contenido
        let eventos = map csvToEventoSeguro lineas
        return (catMaybes eventos)