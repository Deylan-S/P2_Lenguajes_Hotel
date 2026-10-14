module Rates (stayCost) where

import Types
import Dates (dayOfWeek, nextDay)

-- Objetivo: calcular el costo de una habitación por toda la estancia
-- Entradas: lista de tarifas de (1 a 8), fecha de entrada, fecha de salida, 
--           cantidad de adultos, cantidad de niños
-- Salidas: Just el monto total sumando el costo de cada noche, o Nothing si falta alguna tarifa
-- Restricciones: la noche de salida no se cobra
stayCost :: [Rate] -> Date -> Date -> Int -> Int -> Maybe Double
stayCost rates checkIn checkOut adults children =
    fmap sum (mapM (nightCost rates adults children) (getNights checkIn checkOut))

-- Objetivo: crear la lista de fechas que corresponde a las noches a cobrar
-- Entradas: fecha de entrada y fecha de salida
-- Salidas: lista de fechas (Date) desde la entrada hasta el día antes de la salida
-- Restricciones: se asume que el módulo Dates exporta la función nextDay
getNights :: Date -> Date -> [Date]
getNights current end
    | current >= end = []
    | otherwise      = current : getNights (nextDay current) end

-- Objetivo: calcular el costo de una noche específica
-- Entradas: lista de tarifas, cantidad de adultos, cantidad de niños y la fecha de la noche
-- Salidas: Just el costo de esa noche, o Nothing si falta la tarifa de adulto o la de niño
-- Restricciones: busca la tarifa que aplica según la temporada y el tipo de día
nightCost :: [Rate] -> Int -> Int -> Date -> Maybe Double
nightCost rates adults children date = do
    adultPrice <- findRatePrice (getRateId date True) rates
    childPrice <- findRatePrice (getRateId date False) rates
    Just ((fromIntegral adults * adultPrice) + (fromIntegral children * childPrice))

-- Objetivo: determinar el ID de la tarifa de (1 a 8) según la fecha y el tipo de huésped
-- Entradas: la fecha a evaluar y un booleano (True si es adulto, False si es niño)
-- Salidas: el ID de la tarifa correspondiente a las reglas del hotel
-- Restricciones: Verde (abril a octubre), Alta (noviembre a marzo) y fin de semana (viernes a sábado)
getRateId :: Date -> Bool -> Int
getRateId date isAdult =
    let
        -- Temporada Verde: de abril (4) a octubre (10)
        isGreenSeason = month date >= 4 && month date <= 10
        
        -- Fin de semana: de viernes a sábado
        -- NOTA: Se asume que dayOfWeek devuelve 5 para Viernes y 6 para Sábado
        isWeekend = dayOfWeek date == 5 || dayOfWeek date == 6
    in
        case (isGreenSeason, isWeekend, isAdult) of
            (True, False, True)   -> 1 -- Verde entre semana adulto
            (True, False, False)  -> 2 -- Verde entre semana niño
            (True, True, True)    -> 3 -- Verde fin de semana adulto
            (True, True, False)   -> 4 -- Verde fin de semana niño
            (False, False, True)  -> 5 -- Alta entre semana adulto
            (False, False, False) -> 6 -- Alta entre semana niño
            (False, True, True)   -> 7 -- Alta fin de semana adulto
            (False, True, False)  -> 8 -- Alta fin de semana niño

-- Objetivo: buscar el precio de una tarifa por su ID en la base de datos de tarifas
-- Entradas: el ID de la tarifa buscada y la lista de tarifas cargadas
-- Salidas: Just el monto de la tarifa (Double), o Nothing si no está cargada
-- Restricciones: ninguna
findRatePrice :: Int -> [Rate] -> Maybe Double
findRatePrice targetId rates =
    case filter (\r -> rateId r == targetId) rates of
        (r:_) -> Just (rateAmount r)
        []    -> Nothing