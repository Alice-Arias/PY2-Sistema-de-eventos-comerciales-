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
import Control.Exception (try, IOException)
import System.IO.Error ()

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
eventoToCSV evento =
    show (idEvento evento) ++ "," ++
    show (categoria evento) ++ "," ++
    show (valor evento) ++ "," ++
    show (timestamp evento) ++ "," ++
    show (usuarioId evento) ++ "," ++
    show (productoId evento) ++ "," ++
    show (producto evento) ++ "," ++
    show (cantidad evento) ++ "," ++
    show (metodoPago evento) ++ "," ++
    show (estado evento) ++ "," ++
    show (impuesto evento) ++ "," ++
    show (etiqueta evento) ++ "," ++
    show (total evento)

-- Nombre: existeEvento
-- Descripción: Verifica si un evento ya existe en una lista por su ID.
-- Entradas: Int (id a buscar), lista de Evento.
-- Salidas: Bool (True si existe).
-- Validaciones: Compara solo por idEvento, no valida otros campos ni consistencia.
existeEvento :: Int -> [Evento] -> Bool
existeEvento idBuscado = any (\evento -> idEvento evento == idBuscado)


-- Nombre: agregarEventoSeguro
-- Descripción: Agrega un evento a un archivo CSV si es válido y no está duplicado.
-- Entradas: Ruta del archivo (FilePath), Evento.
-- Salidas: IO () con mensaje en consola.
-- Validaciones: Verifica duplicados por ID, valida campos del evento, maneja errores de archivo. Solo agrega si el evento es válido y no existe.
agregarEventoSeguro rutaArchivo nuevoEvento = do

    eventosExistentes <- leerEventosSeguro rutaArchivo

    if existeEvento (idEvento nuevoEvento) eventosExistentes

        then putStrLn "Error: evento duplicado"

        else case validarEvento nuevoEvento of

            Left err -> putStrLn ("Error: " ++ err)

            Right eventoValido -> do

                resultado <- try (appendFile rutaArchivo (eventoToCSV eventoValido ++ "\n"))
                    :: IO (Either IOException ())
                    
                case resultado of
                    Left _ -> putStrLn "Error al guardar el archivo"
                    Right _ -> return ()

-- Nombre: split
-- Descripción: Divide un texto usando un separador.
-- Entradas: Char , String.
-- Salidas: Lista de String.
-- Validaciones: No valida contenido, solo divide. Si el separador no está, devuelve la lista con el texto completo.
split :: Char -> String -> [String]
split _ [] = []

split separador texto =
    let (parteAntes, restoTexto) = break (== separador) texto

        listaRestante =
                        if null restoTexto
                        then []
                        else split separador (tail restoTexto)

    in parteAntes : listaRestante

-- Nombre: csvToEventoSeguro
-- Descripción: Convierte una línea CSV a Evento si es válida.
-- Entradas: String (línea CSV).
-- Salidas: Maybe Evento.
-- Validaciones: Verifica que tenga 13 campos, que los tipos sean correctos. Devuelve Nothing si no es válido, Just Evento si es correcto. No valida valores específicos, solo formato y tipos.
csvToEventoSeguro :: String -> Maybe Evento
csvToEventoSeguro lineaCSV =
    let campos = split ',' lineaCSV
    in if length campos /= 13
        then Nothing
        else Just (Evento
            (read (head campos))           -- idEvento
            (read (campos !! 1))           -- categoria
            (read (campos !! 2))           -- valor
            (read (campos !! 3))           -- timestamp
            (read (campos !! 4))           -- usuarioId
            (read (campos !! 5))           -- productoId
            (read (campos !! 6))                  -- producto
            (read (campos !! 7))           -- cantidad
            (read (campos !! 8))           -- metodoPago
            (read (campos !! 9))           -- estado
            (read (campos !! 10))          -- impuesto
            (read (campos !! 11))          -- etiqueta
            (read (campos !! 12))          -- total
        )

-- NOMBRE: leerEventosSeguro
-- DESCRIPCIÓN: Lee un archivo CSV y devuelve una lista de eventos válidos.
-- ENTRADAS: Ruta del archivo
-- Salidas: IO [Evento]
-- Validaciones: Si el archivo no existe devuelve lista vacía. Ignora líneas inválidas. Solo devuelve eventos que se puedan convertir correctamente. No valida valores específicos, solo formato y tipos.
leerEventosSeguro :: FilePath -> IO [Evento]
leerEventosSeguro rutaArchivo = do

    archivoExiste <- doesFileExist rutaArchivo

    if not archivoExiste
        then return []
        else do
            contenido <- readFile rutaArchivo
            let lineas = lines contenido
            let eventosParseados = map csvToEventoSeguro lineas
            return (catMaybes eventosParseados)


-- Nombre: guardarEventos
-- Descripción: Guarda una lista de eventos en un archivo CSV, sobrescribiendo el contenido existente.
-- Entradas: Ruta del archivo (FilePath), lista de eventos.
-- Salidas: IO () con mensaje en consola.
-- Validaciones: Maneja errores de archivo. Solo guarda eventos válidos. Si el archivo
guardarEventos :: FilePath -> [Evento] -> IO ()
guardarEventos rutaArchivo eventos = writeFile rutaArchivo (unlines (map eventoToCSV eventos))