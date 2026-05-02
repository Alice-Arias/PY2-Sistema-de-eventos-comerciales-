module Utils.FechaSegura where

import Data.Char (isDigit)

parseFecha :: String -> Maybe Int
parseFecha input =
    case split '-' input of
        [dd, mm, yyyy]
            | esFechaValida dd mm yyyy ->
                Just (toInt yyyy * 10000 + toInt mm * 100 + toInt dd)
        _ -> Nothing



esFechaValida :: String -> String -> String -> Bool
esFechaValida dd mm yyyy =
    soloDigitos dd &&
    soloDigitos mm &&
    soloDigitos yyyy &&
    not (null dd || null mm || null yyyy) &&
    length yyyy == 4 &&
    dentroRango (toInt dd) 1 31 &&
    dentroRango (toInt mm) 1 12

soloDigitos :: String -> Bool
soloDigitos = all isDigit

dentroRango :: Int -> Int -> Int -> Bool
dentroRango x min max = x >= min && x <= max

toInt :: String -> Int
toInt s = read s


split :: Char -> String -> [String]
split _ [] = [""]
split delim str = go str ""
  where
    go [] acc = [reverse acc]
    go (c:cs) acc
        | c == delim = reverse acc : go cs ""
        | otherwise  = go cs (c:acc)