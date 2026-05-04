module Utils.CSV where

import Data.Maybe (catMaybes)
import Types.Modelos
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (try, IOException)
import System.IO.Error ()

--------------------------------------------------------------------------------
-- Nombre: validarEvento
-- Entrada:
--   evento: evento a validar
-- Salida:
--   Either con error o evento válido
-- Restricciones:
--   - Valida valores numéricos básicos del evento
--------------------------------------------------------------------------------
validarEvento :: Evento -> Either String Evento
validarEvento evento
    | valor evento <= 0 = Left "Valor inválido"
    | cantidad evento <= 0 = Left "Cantidad inválida"
    | impuesto evento < 0 = Left "Impuesto inválido"
    | otherwise = Right evento

--------------------------------------------------------------------------------
-- Nombre: eventoToCSV
-- Entrada:
--   evento: evento del sistema
-- Salida:
--   representación del evento en formato CSV
-- Restricciones:
--   - Convierte todos los campos a String
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Nombre: existeEvento
-- Entrada:
--   idBuscado: identificador del evento
--   listaEventos: lista de eventos
-- Salida:
--   True si el evento existe, False si no
-- Restricciones:
--   - Compara por idEvento
--------------------------------------------------------------------------------
existeEvento :: Int -> [Evento] -> Bool
existeEvento idBuscado = any (\eventoActual -> idEvento eventoActual == idBuscado)

--------------------------------------------------------------------------------
-- Nombre: agregarEventoSeguro
-- Entrada:
--   rutaArchivo: archivo CSV destino
--   nuevoEvento: evento a insertar
-- Salida:
--   acción IO con resultado en consola
-- Restricciones:
--   - Evita duplicados
--   - Valida evento antes de guardar
--   - Maneja errores de archivo
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Nombre: split
-- Entrada:
--   separador: carácter divisor
--   texto: cadena a dividir
-- Salida:
--   lista de subcadenas
-- Restricciones:
--   - Divide texto por un delimitador
--------------------------------------------------------------------------------
split :: Char -> String -> [String]
split _ [] = []

split separador texto =
    let
        (parteAntesSeparador, restoDelTexto) = break (== separador) texto

        listaRestante =
            if null restoDelTexto
            then []
            else split separador (tail restoDelTexto)

    in
        parteAntesSeparador : listaRestante

--------------------------------------------------------------------------------
-- Nombre: csvToEventoSeguro
-- Entrada:
--   lineaCSV: línea de texto CSV
-- Salida:
--   Maybe Evento (Nothing si falla parseo)
-- Restricciones:
--   - Debe tener exactamente 13 campos
--------------------------------------------------------------------------------
csvToEventoSeguro :: String -> Maybe Evento
csvToEventoSeguro lineaCSV =
    let campos = split ',' lineaCSV
    in if length campos /= 13
        then Nothing
        else Just (Evento
            (read (head campos))
            (read (campos !! 1))
            (read (campos !! 2))
            (read (campos !! 3))
            (read (campos !! 4))
            (read (campos !! 5))
            (read (campos !! 6))
            (read (campos !! 7))
            (read (campos !! 8))
            (read (campos !! 9))
            (read (campos !! 10))
            (read (campos !! 11))
            (read (campos !! 12))
        )

--------------------------------------------------------------------------------
-- Nombre: leerEventosSeguro
-- Entrada:
--   rutaArchivo: ruta del archivo CSV
-- Salida:
--   lista de eventos leídos
-- Restricciones:
--   - Ignora líneas inválidas
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Nombre: guardarEventos
-- Entrada:
--   rutaArchivo: archivo destino
--   eventos: lista de eventos
-- Salida:
--   escritura en archivo
-- Restricciones:
--   - Sobrescribe archivo completo
--------------------------------------------------------------------------------
guardarEventos :: FilePath -> [Evento] -> IO ()
guardarEventos rutaArchivo eventos =
    writeFile rutaArchivo (unlines (map eventoToCSV eventos))