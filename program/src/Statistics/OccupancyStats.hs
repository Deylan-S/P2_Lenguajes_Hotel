module Statistics.OccupancyStats
    ( countedReservations
    , guestsByMonthYear
    , collectedAmounts
    , weekdayCounts
    , topWeekdays
    ) where

import Data.List (nub, sort, sortBy)

import Types
import Utils.DateUtils (datesBetween, dayOfWeek)

-- Objetivo: obtener las reservaciones que cuentan para las estadisticas
-- Entradas: lista de reservaciones
-- Salidas: las reservaciones activas y facturadas (se ignoran las canceladas)
-- Restricciones: ninguna
countedReservations :: [Reservation] -> [Reservation]
countedReservations = filter ((/= Cancelled) . resStatus)

-- Objetivo: calcular el total de huespedes por mes y anio
-- Entradas: lista de reservaciones
-- Salidas: lista de ((anio, mes), huespedes) ordenada cronologicamente
-- Restricciones: cada reservacion se cuenta en el mes de su fecha de ingreso; se ignoran las canceladas
guestsByMonthYear :: [Reservation] -> [((Int, Int), Int)]
guestsByMonthYear reservations =
    [ (key, sum [ guests | (otherKey, guests) <- entries, otherKey == key ])
    | key <- sort (nub (map fst entries)) ]
  where
    entries = [ (monthKey (resCheckIn r), resAdults r + resChildren r)
              | r <- countedReservations reservations ]
    monthKey (Date y m _) = (y, m)

-- Objetivo: calcular el monto recaudado con impuestos
-- Entradas: porcentaje del impuesto de ventas (por ejemplo 0.13) y lista de reservaciones
-- Salidas: (subtotal, impuesto, total con impuesto)
-- Restricciones: se ignoran las reservaciones canceladas; el impuesto se redondea a dos
--                decimales por reservacion, igual que en la factura
collectedAmounts :: Double -> [Reservation] -> (Double, Double, Double)
collectedAmounts taxRate reservations =
    (subtotal, tax, subtotal + tax)
  where
    counted  = countedReservations reservations
    subtotal = sum (map resTotal counted)
    tax      = sum (map (roundTwoDecimals . (* taxRate) . resTotal) counted)

-- Objetivo: redondear un monto a dos decimales
-- Entradas: un monto
-- Salidas: el monto redondeado
-- Restricciones: ninguna
roundTwoDecimals :: Double -> Double
roundTwoDecimals amount = fromIntegral (round (amount * 100) :: Integer) / 100

-- Objetivo: contar las habitaciones ocupadas por cada dia de la semana
-- Entradas: lista de reservaciones
-- Salidas: lista de (dia de la semana 0 a 6, habitaciones ocupadas) con los 7 dias
-- Restricciones: cada noche de cada reservacion cuenta una vez por habitacion; se ignoran las canceladas
weekdayCounts :: [Reservation] -> [(Int, Int)]
weekdayCounts reservations =
    [ (weekday, sum [ rooms | (w, rooms) <- contributions, w == weekday ]) | weekday <- [0 .. 6] ]
  where
    contributions = concatMap contributionOf (countedReservations reservations)
    contributionOf r =
        [ (dayOfWeek night, length (resOccupancies r))
        | night <- datesBetween (resCheckIn r) (resCheckOut r) ]

-- Objetivo: obtener los dias de la semana con mas ocupaciones
-- Entradas: cantidad de dias a mostrar y lista de reservaciones
-- Salidas: lista de (dia de la semana, habitaciones ocupadas) de mayor a menor
-- Restricciones: omite los dias sin ocupaciones; en empates gana el dia que aparece primero (domingo a sabado)
topWeekdays :: Int -> [Reservation] -> [(Int, Int)]
topWeekdays limit reservations =
    take limit (filter ((> 0) . snd) (sortBy compareCounts (weekdayCounts reservations)))
  where
    compareCounts (dayA, countA) (dayB, countB) = compare (countB, dayA) (countA, dayB)
