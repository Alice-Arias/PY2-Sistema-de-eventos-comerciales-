module Utils.CSV where
import Data.Maybe (catMaybes)
import Types.Evento
import System.IO
import System.Directory (doesFileExist)
import Control.Exception


validarEvento :: Evento -> Either String Evento
validarEvento e
    | valor e <= 0 = Left "Valor inválido"
    | cantidad e <= 0 = Left "Cantidad inválida"
    | impuesto e < 0 = Left "Impuesto inválido"
    | otherwise = Right e


eventoToCSV :: Evento -> String
eventoToCSV e =
    show (idE e) ++ "," ++
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

existeEvento :: Int -> [Evento] -> Bool
existeEvento idBuscar = any (\e -> idE e == idBuscar)

agregarEventoSeguro :: FilePath -> Evento -> IO ()
agregarEventoSeguro ruta evento = do
  eventos <- leerEventosSeguro ruta

  if existeEvento (idE evento) eventos
    then putStrLn "Error: evento duplicado"
    else
      case validarEvento evento of
        Left errorMsg -> putStrLn ("Error: " ++ errorMsg)
        Right e -> do
          resultado <- try (appendFile ruta (eventoToCSV e ++ "\n")) :: IO (Either IOException ())
          case resultado of
            Left _ -> putStrLn "Error al guardar el archivo"
            Right _ -> putStrLn "Evento guardado correctamente"


split :: Char -> String -> [String]
split _ [] = []
split delim str =
    let (word, rest) = break (== delim) str
    in word : case rest of
        [] -> []
        (_:xs) -> split delim xs


csvToEventoSeguro :: String -> Maybe Evento
csvToEventoSeguro line =
    let c = split ',' line
    in if length c /= 11
        then Nothing
        else Just (Evento
        (read (head c))
        (read (c !! 1))
        (read (c !! 2))
        (read (c !! 3))
        (read (c !! 4))
        (read (c !! 5))
        (c !! 6)
        (read (c !! 7))
        (read (c !! 8))
        (read (c !! 9))
        (read (c !! 10)))


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