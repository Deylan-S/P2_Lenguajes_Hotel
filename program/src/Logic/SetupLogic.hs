module Logic.SetupLogic
    ( maxRoomsPerType
    , maxGuestsLimit
    , normalizeRoomType
    , isValidRoomType
    , splitNewRoomTypes
    , assignmentBlocker
    , buildRoomPlan
    , isValidRateSet
    ) where

import Data.Char (toLower)
import Data.List (sort)

import Types
import Utils.TextUtils (isBlank, trim)

-- Objetivo: limite de habitaciones que se pueden crear por tipo
-- Entradas: ninguna
-- Salidas: el limite
-- Restricciones: evita cantidades enormes por errores de digitacion
maxRoomsPerType :: Int
maxRoomsPerType = 500

-- Objetivo: limite de huespedes que puede tener un tipo de habitacion
-- Entradas: ninguna
-- Salidas: el limite
-- Restricciones: evita valores enormes en el archivo de tipos
maxGuestsLimit :: Int
maxGuestsLimit = 50

-- Objetivo: quitar espacios sobrantes del nombre y la descripcion de un tipo
-- Entradas: un tipo de habitacion
-- Salidas: el tipo con textos limpios
-- Restricciones: ninguna
normalizeRoomType :: RoomType -> RoomType
normalizeRoomType (RoomType name description maxGuests) =
    RoomType (trim name) (trim description) maxGuests

-- Objetivo: validar un tipo de habitacion leido de un archivo
-- Entradas: un tipo de habitacion
-- Salidas: True si el nombre no esta vacio y los huespedes estan entre 1 y el limite
-- Restricciones: ninguna
isValidRoomType :: RoomType -> Bool
isValidRoomType roomTypeValue =
    not (isBlank (rtName roomTypeValue))
        && rtMaxGuests roomTypeValue >= 1
        && rtMaxGuests roomTypeValue <= maxGuestsLimit

-- Objetivo: separar los tipos nuevos de los que ya existen
-- Entradas: tipos ya registrados y tipos candidatos
-- Salidas: (tipos nuevos sin repetir, tipos repetidos)
-- Restricciones: la comparacion de nombres ignora mayusculas y minusculas
splitNewRoomTypes :: [RoomType] -> [RoomType] -> ([RoomType], [RoomType])
splitNewRoomTypes existing candidates =
    go (map (lowerName . rtName) existing) candidates [] []
  where
    lowerName = map toLower
    go _ [] fresh repeated = (reverse fresh, reverse repeated)
    go seen (candidate : rest) fresh repeated
        | key `elem` seen = go seen rest fresh (candidate : repeated)
        | otherwise       = go (key : seen) rest (candidate : fresh) repeated
      where
        key = lowerName (rtName candidate)

-- Objetivo: determinar si no se pueden generar habitaciones todavia
-- Entradas: habitaciones existentes y tipos registrados
-- Salidas: Just mensaje con el motivo, o Nothing si se puede continuar
-- Restricciones: las habitaciones solo se generan una vez
assignmentBlocker :: [Room] -> [RoomType] -> Maybe String
assignmentBlocker existingRooms roomTypes
    | not (null existingRooms) =
        Just "Las habitaciones ya fueron generadas."
    | null roomTypes =
        Just "Primero debe cargar los tipos de habitaciones."
    | otherwise = Nothing

-- Objetivo: armar el plan de habitaciones a generar
-- Entradas: tipos de habitacion y la cantidad indicada para cada uno
-- Salidas: lista de (nombre del tipo, cantidad) solo con cantidades mayores a cero
-- Restricciones: las listas deben estar en el mismo orden
buildRoomPlan :: [RoomType] -> [Int] -> [(String, Int)]
buildRoomPlan roomTypes quantities =
    filter ((> 0) . snd) (zip (map rtName roomTypes) quantities)

-- Objetivo: validar que un conjunto de tarifas cargado desde un archivo este completo
-- Entradas: lista de tarifas
-- Salidas: True si tiene exactamente los ids 1 a 8 y montos no negativos
-- Restricciones: ninguna
isValidRateSet :: [Rate] -> Bool
isValidRateSet rates =
    sort (map rateId rates) == [1 .. 8] && all ((>= 0) . rateAmount) rates
