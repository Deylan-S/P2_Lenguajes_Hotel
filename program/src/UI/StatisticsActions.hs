module UI.StatisticsActions
    ( actionStatGuests
    , actionStatOccupied
    , actionStatUnoccupied
    , actionStatTaxes
    , actionStatTopDays
    ) where

import Data.List (sortOn)

import Availability (availableRooms, occupiedRooms)
import Billing (taxRate)
import ReservationDM (getReservations)
import Reservations (formatMoney)
import Statistics.OccupancyStats
import Types
import UI.Common (withGeneratedRooms)
import UI.Display (printError, printLines, printPanel)
import UI.Formatters
import UI.Input (readValidDate)
import Utils.DateUtils (formatDateWithWeekday)

-- Objetivo: estadistica del total de huespedes por mes y anio
-- Entradas: ninguna
-- Salidas: imprime una tabla mes-anio y huespedes
-- Restricciones: se ignoran las reservaciones canceladas
actionStatGuests :: IO ()
actionStatGuests = do
    printPanel "TOTAL DE HUÉSPEDES POR MES-AÑO"
    reservations <- getReservations
    let totals = guestsByMonthYear reservations
    if null totals
        then putStrLn "\nNo hay reservaciones para calcular la estadística."
        else do
            putStrLn ""
            printLines (formatTable ["Mes-Año", "Huéspedes"] (map guestsRow totals))
            putStrLn ("\nTotal general de huéspedes: " ++ show (sum (map snd totals)))
            putStrLn "(Los huéspedes se cuentan en el mes de su fecha de ingreso)"

-- Objetivo: estadistica del listado de habitaciones ocupadas en un dia
-- Entradas: pide la fecha al usuario
-- Salidas: imprime las habitaciones ocupadas y su reservacion
-- Restricciones: requiere habitaciones generadas
actionStatOccupied :: IO ()
actionStatOccupied = do
    printPanel "HABITACIONES OCUPADAS EN UN DÍA"
    withGeneratedRooms (\_ -> do
        date <- readValidDate "Día a consultar (dd/mm/aaaa): "
        busyRooms <- occupiedRooms date
        putStrLn ("\nHabitaciones ocupadas el " ++ formatDateWithWeekday date ++ ": " ++ show (length busyRooms))
        printLines (tableOrNone ["ID", "Tipo", "Reserva"] (map occupiedRoomRow (sortOn (roomId . fst) busyRooms))))

-- Objetivo: estadistica del total de habitaciones no ocupadas en un dia
-- Entradas: pide la fecha al usuario
-- Salidas: imprime la cantidad de habitaciones libres
-- Restricciones: requiere habitaciones generadas
actionStatUnoccupied :: IO ()
actionStatUnoccupied = do
    printPanel "HABITACIONES NO OCUPADAS EN UN DÍA"
    withGeneratedRooms (\allRooms -> do
        date <- readValidDate "Día a consultar (dd/mm/aaaa): "
        freeRooms <- availableRooms date
        putStrLn ("\nHabitaciones no ocupadas el " ++ formatDateWithWeekday date ++ ": "
                  ++ show (length freeRooms) ++ " de " ++ show (length allRooms)))

-- Objetivo: estadistica del monto recaudado con impuestos
-- Entradas: ninguna
-- Salidas: imprime subtotal, impuesto y total
-- Restricciones: se ignoran las reservaciones canceladas
actionStatTaxes :: IO ()
actionStatTaxes = do
    printPanel "MONTO RECAUDADO CON IMPUESTOS"
    reservations <- getReservations
    let counted = countedReservations reservations
        (subtotal, tax, total) = collectedAmounts taxRate reservations
    if null counted
        then putStrLn "\nNo hay reservaciones (no canceladas) para calcular el monto."
        else do
            putStrLn ""
            putStrLn ("Reservaciones consideradas : " ++ show (length counted))
            putStrLn ("Subtotal (sin impuesto)    : " ++ formatMoney subtotal)
            putStrLn ("Impuesto de ventas         : " ++ formatMoney tax)
            putStrLn ("Total con impuestos        : " ++ formatMoney total)

-- Objetivo: estadistica de los 3 dias de la semana con mas ocupaciones
-- Entradas: ninguna
-- Salidas: imprime el ranking
-- Restricciones: se cuentan habitaciones ocupadas por noche; se ignoran las canceladas
actionStatTopDays :: IO ()
actionStatTopDays = do
    printPanel "TOP 3 DÍAS DE LA SEMANA CON MÁS OCUPACIONES"
    reservations <- getReservations
    let ranking = topWeekdays 3 reservations
    if null ranking
        then printError "No hay ocupaciones registradas."
        else do
            putStrLn ""
            printLines (formatTable ["Puesto", "Día", "Habitaciones ocupadas"]
                                    (zipWith weekdayRow [1 ..] ranking))
