module Utils.DateUtils
    ( isLeapYear
    , daysInMonth
    , isValidDate
    , parseDate
    , formatDate
    , formatDateTime
    , formatDateWithWeekday
    , dateToDayNumber
    , dayNumberToDate
    , addDays
    , dayOfWeek
    , weekdayName
    , datesBetween
    , nightsBetween
    , getCurrentDateTime
    ) where

import Data.Time (getZonedTime, localDay, localTimeOfDay, toGregorian, todHour, todMin, zonedTimeToLocalTime)
import Text.Read (readMaybe)

import Types
import Utils (splitOnChar)
import Utils.TextUtils (pad2, trim)

-- Objetivo: saber si un anio es bisiesto
-- Entradas: un anio
-- Salidas: True si es bisiesto
-- Restricciones: ninguna
isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0

-- Objetivo: obtener la cantidad de dias de un mes
-- Entradas: anio y mes (1 a 12)
-- Salidas: cantidad de dias del mes
-- Restricciones: el mes debe estar entre 1 y 12
daysInMonth :: Int -> Int -> Int
daysInMonth y m
    | m == 2               = if isLeapYear y then 29 else 28
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise            = 31

-- Objetivo: validar que una fecha exista en el calendario
-- Entradas: una fecha
-- Salidas: True si el anio esta entre 1900 y 2100 y el dia existe en ese mes
-- Restricciones: ninguna
isValidDate :: Date -> Bool
isValidDate (Date y m d) =
    y >= 1900 && y <= 2100 && m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth y m

-- Objetivo: convertir un texto en una fecha
-- Entradas: texto con formato dd/mm/aaaa
-- Salidas: Just fecha si el texto es valido, Nothing en caso contrario
-- Restricciones: la fecha debe existir en el calendario
parseDate :: String -> Maybe Date
parseDate text =
    case splitOnChar '/' (trim text) of
        [dayText, monthText, yearText] -> do
            d <- readMaybe dayText
            m <- readMaybe monthText
            y <- readMaybe yearText
            let date = Date y m d
            if isValidDate date then Just date else Nothing
        _ -> Nothing

-- Objetivo: mostrar una fecha como texto
-- Entradas: una fecha
-- Salidas: texto con formato dd/mm/aaaa
-- Restricciones: ninguna
formatDate :: Date -> String
formatDate (Date y m d) = pad2 d ++ "/" ++ pad2 m ++ "/" ++ show y

-- Objetivo: mostrar una fecha con hora como texto
-- Entradas: una fecha con hora
-- Salidas: texto con formato dd/mm/aaaa hh:mm
-- Restricciones: ninguna
formatDateTime :: DateTime -> String
formatDateTime (DateTime date hour minute) =
    formatDate date ++ " " ++ pad2 hour ++ ":" ++ pad2 minute

-- Objetivo: mostrar una fecha junto al nombre de su dia de la semana
-- Entradas: una fecha
-- Salidas: texto con formato dd/mm/aaaa (Dia)
-- Restricciones: ninguna
formatDateWithWeekday :: Date -> String
formatDateWithWeekday date = formatDate date ++ " (" ++ weekdayName (dayOfWeek date) ++ ")"

-- Objetivo: convertir una fecha en la cantidad de dias desde el 01/01/1970
-- Entradas: una fecha valida
-- Salidas: numero de dia (0 es el 01/01/1970)
-- Restricciones: la fecha debe ser valida
dateToDayNumber :: Date -> Int
dateToDayNumber (Date y m d) =
    let adjustedYear = if m <= 2 then y - 1 else y
        era = adjustedYear `div` 400
        yearOfEra = adjustedYear - era * 400
        monthIndex = (m + 9) `mod` 12
        dayOfYear = (153 * monthIndex + 2) `div` 5 + d - 1
        dayOfEra = yearOfEra * 365 + yearOfEra `div` 4 - yearOfEra `div` 100 + dayOfYear
    in era * 146097 + dayOfEra - 719468

-- Objetivo: convertir un numero de dia en una fecha
-- Entradas: dias desde el 01/01/1970
-- Salidas: la fecha correspondiente
-- Restricciones: es la inversa de dateToDayNumber
dayNumberToDate :: Int -> Date
dayNumberToDate n =
    let shifted = n + 719468
        era = shifted `div` 146097
        dayOfEra = shifted - era * 146097
        yearOfEra = (dayOfEra - dayOfEra `div` 1460 + dayOfEra `div` 36524 - dayOfEra `div` 146096) `div` 365
        baseYear = yearOfEra + era * 400
        dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra `div` 4 - yearOfEra `div` 100)
        monthIndex = (5 * dayOfYear + 2) `div` 153
        d = dayOfYear - (153 * monthIndex + 2) `div` 5 + 1
        m = if monthIndex < 10 then monthIndex + 3 else monthIndex - 9
        y = if m <= 2 then baseYear + 1 else baseYear
    in Date y m d

-- Objetivo: sumar dias a una fecha
-- Entradas: cantidad de dias (puede ser negativa) y una fecha
-- Salidas: la nueva fecha
-- Restricciones: ninguna
addDays :: Int -> Date -> Date
addDays amount date = dayNumberToDate (dateToDayNumber date + amount)

-- Objetivo: obtener el dia de la semana de una fecha
-- Entradas: una fecha valida
-- Salidas: 0 domingo, 1 lunes, ... 6 sabado
-- Restricciones: la fecha debe ser valida
dayOfWeek :: Date -> Int
dayOfWeek date = (dateToDayNumber date + 4) `mod` 7

-- Objetivo: obtener el nombre de un dia de la semana
-- Entradas: indice del dia (0 domingo a 6 sabado)
-- Salidas: nombre del dia
-- Restricciones: el indice se ajusta con modulo 7
weekdayName :: Int -> String
weekdayName index =
    ["Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"] !! (index `mod` 7)

-- Objetivo: listar las noches de una estadia
-- Entradas: fecha de ingreso y fecha de salida
-- Salidas: fechas desde el ingreso hasta el dia anterior a la salida
-- Restricciones: si la salida no es posterior al ingreso devuelve lista vacia
datesBetween :: Date -> Date -> [Date]
datesBetween from to =
    map dayNumberToDate [dateToDayNumber from .. dateToDayNumber to - 1]

-- Objetivo: contar las noches de una estadia
-- Entradas: fecha de ingreso y fecha de salida
-- Salidas: cantidad de noches
-- Restricciones: nunca devuelve un valor negativo
nightsBetween :: Date -> Date -> Int
nightsBetween from to = max 0 (dateToDayNumber to - dateToDayNumber from)

-- Objetivo: obtener la fecha y hora actual del sistema
-- Entradas: ninguna
-- Salidas: fecha y hora local
-- Restricciones: ninguna
getCurrentDateTime :: IO DateTime
getCurrentDateTime = do
    now <- getZonedTime
    let localTime = zonedTimeToLocalTime now
        (y, m, d) = toGregorian (localDay localTime)
        timeOfDay = localTimeOfDay localTime
    pure (DateTime (Date (fromInteger y) m d) (todHour timeOfDay) (todMin timeOfDay))
