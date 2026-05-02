
module Utils.CSV where

import Data.Maybe (catMaybes)
import Types.Evento
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (try, IOException)
import System.IO.Error ()


validarEvento :: Evento -> Either String Evento
validarEvento evento
    | valor evento <= 0 = Left "Valor inválido"
    | cantidad evento <= 0 = Left "Cantidad inválida"
    | impuesto evento < 0 = Left "Impuesto inválido"
    | otherwise = Right evento


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

split :: Char -> String -> [String]
split _ [] = []

split separador texto =
    let (parteAntes, restoTexto) = break (== separador) texto

        listaRestante =
                        if null restoTexto
                        then []
                        else split separador (tail restoTexto)

    in parteAntes : listaRestante

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


guardarEventos :: FilePath -> [Evento] -> IO ()
guardarEventos rutaArchivo eventos = writeFile rutaArchivo (unlines (map eventoToCSV eventos))