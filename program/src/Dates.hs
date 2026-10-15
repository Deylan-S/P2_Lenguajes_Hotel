module Dates
    ( daysInMonth
    , isValidDate
    , nextDay
    , dayOfWeek
    , weekdayName
    , nightsOf
    , formatDate
    , parseDate
    ) where

import Types
import Text.Read (readMaybe)

-- Objetivo: obtener la cantidad de días de un mes
-- Entradas: un mes (1 a 12)
-- Salidas: 28 para febrero, 30 para abril, junio, septiembre y noviembre, 31 para los demás
-- Restricciones: el mes debe estar entre 1 y 12
daysInMonth :: Int -> Int
daysInMonth m
    | m == 2                = 28
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise             = 31

-- Objetivo: saber si una fecha existe en el calendario
-- Entradas: una fecha
-- Salidas: True si el año es positivo, el mes está entre 1 y 12 y el día existe en ese mes
-- Restricciones: ninguna
isValidDate :: Date -> Bool
isValidDate (Date y m d) =
    if y >= 1 && m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth m
    then True
    else False

-- Objetivo: obtener el día siguiente a una fecha
-- Entradas: una fecha válida
-- Salidas: la fecha del día siguiente (cambia de mes y de año cuando corresponde)
-- Restricciones: la fecha debe ser válida
nextDay :: Date -> Date
nextDay (Date y m d)
    | d < daysInMonth m = Date y m (d + 1)
    | m < 12              = Date y (m + 1) 1
    | otherwise           = Date (y + 1) 1 1

-- Objetivo: calcular el día de la semana de una fecha
-- Entradas: una fecha válida
-- Salidas: 0 = domingo, 1 = lunes, 2 = martes, 3 = miércoles, 4 = jueves, 5 = viernes, 6 = sábado
-- Restricciones: la fecha debe ser válida
dayOfWeek :: Date -> Int
dayOfWeek (Date y m d) =
    (y' + y' `div` 4 - y' `div` 100 + y' `div` 400 + offsets !! (m - 1) + d) `mod` 7
  where
    offsets = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
    y' = if m < 3 then y - 1 else y

-- Objetivo: obtener el nombre de un día de la semana para mostrarlo en pantalla
-- Entradas: un número de día (0 = domingo ... 6 = sábado)
-- Salidas: el nombre del día en español
-- Restricciones: ninguna
weekdayName :: Int -> String
weekdayName n = names !! (n `mod` 7)
  where
    names = ["Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"]

-- Objetivo: listar las noches de una estadía
-- Entradas: fecha de entrada y fecha de salida
-- Salidas: todas las fechas desde la entrada hasta el día anterior a la salida
-- Restricciones: si la salida no es posterior a la entrada devuelve una lista vacía
nightsOf :: Date -> Date -> [Date]
nightsOf checkIn checkOut = takeWhile (< checkOut) (iterate nextDay checkIn)

-- Objetivo: dar formato a una fecha para mostrarla en pantalla
-- Entradas: una fecha
-- Salidas: texto con el formato dd/mm/aaaa
-- Restricciones: ninguna
formatDate :: Date -> String
formatDate (Date y m d) = pad2 d ++ "/" ++ pad2 m ++ "/" ++ show y
  where
    pad2 n = if n < 10 then '0' : show n else show n

-- Objetivo: convertir un texto escrito por el usuario en una fecha
-- Entradas: texto con el formato dd/mm/aaaa (se aceptan días y meses de un dígito)
-- Salidas: Just la fecha si el texto tiene tres números y la fecha existe, Nothing en cualquier otro caso
-- Restricciones: ninguna
parseDate :: String -> Maybe Date
parseDate text =
    case splitSlash text of
        [d, m, y] -> do
            dd <- readMaybe d
            mm <- readMaybe m
            yy <- readMaybe y
            let date = Date yy mm dd
            if isValidDate date then Just date else Nothing
        _ -> Nothing

-- Objetivo: partir un texto en las partes separadas por '/'
-- Entradas: un texto
-- Salidas: lista con las partes (un texto sin '/' devuelve una sola parte)
-- Restricciones: ninguna
splitSlash :: String -> [String]
splitSlash s =
    case break (== '/') s of
        (part, '/' : rest) -> part : splitSlash rest
        (part, _)          -> [part]