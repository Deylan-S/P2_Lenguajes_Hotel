module Availability (availableRooms, occupiedRooms, freeInRange) where

import Types
import ReservationDM (getReservations)
import RoomDM (getRooms)

-- Objetivo: saber si una reserva bloquea habitaciones o si el estado es Cancelled
-- Entradas: una reserva
-- Salidas: True si está Active o Invoiced, False si está Cancelled
-- Restricciones: ninguna
blocksRooms :: Reservation -> Bool
blocksRooms r =
    if resStatus r /= Cancelled
    then True
    else False

-- Objetivo: saber si una reserva choca con un día en específico
-- Entradas: una fecha y una reserva
-- Salidas: True si la entrada <= fecha < salida (el día de la salida queda libre)
-- Restricciones: ninguna
coversDate :: Date -> Reservation -> Bool
coversDate d r =
    if (resCheckIn r <= d) && (d < resCheckOut r)
    then True
    else False

-- Objetivo: saber si una reserva choca con un rango de noches
-- Entradas: fecha de entrada, fecha de salida y una reserva
-- Salidas: True si coincide con al menos una noche con (entrada, salida)
-- Restricciones: se asume que la entrada es antes que la salida
overlapsRange :: Date -> Date -> Reservation -> Bool
overlapsRange from to r =
    if (resCheckIn r < to) && (from < resCheckOut r)
    then True
    else False

-- Objetivo: obtener los datos (id de habitación y id de reserva) de las
--           reservas que bloquean habitaciones
-- Entradas: una lista de reservas y una condición sobre reservas
-- Salidas:  lista de datos (id de habitación y id de reserva)
-- Restricciones: ignora las reservas canceladas
bookedPairs :: [Reservation] -> (Reservation -> Bool) -> [(Int, Int)]
bookedPairs reservations covers =
    let filteredReservations = filter (\r -> if blocksRooms r && covers r then True else False) reservations
    in concatMap pairsOf filteredReservations
  where
    pairsOf r = map (\o -> (occRoomId o, resId r)) (resOccupancies r)

-- funciones que se deben exportar

-- Objetivo: listar las habitaciones libres en un día específico
-- Entradas: la fecha a consultar
-- Salidas: lista de habitaciones (Room) disponibles ese día
-- Restricciones: si no se han generado habitaciones, devuelve una lista vacía
availableRooms :: Date -> IO [Room]
availableRooms date = do
    roomsByType <- getRooms
    reservations <- getReservations
    let rooms = concat roomsByType
        busy = map fst (bookedPairs reservations (coversDate date))
    pure (filter (\room -> roomId room `notElem` busy) rooms)

-- Objetivo: listar las habitaciones ocupadas en un día y la reserva que las ocupa
-- Entradas: la fecha a consultar
-- Salidas: lista de datos (habitación, código de reserva)
-- Restricciones: las reservas canceladas no cuentan, las facturadas sí
occupiedRooms :: Date -> IO [(Room, Int)]
occupiedRooms date = do
    roomsByType <- getRooms
    reservations <- getReservations
    let rooms = concat roomsByType
        booked = bookedPairs reservations (coversDate date)
        withReservation room = [ (room, rid) | (rmId, rid) <- booked, rmId == roomId room ]
    pure (concatMap withReservation rooms)

-- Objetivo: listar las habitaciones libres durante todo un rango de noches
-- Entradas: la fecha de entrada y la fecha de salida
-- Salidas: lista de habitaciones libres cada noche del rango
-- Restricciones: la entrada debe ser menor que la salida, sino devuelve una lista vacía
freeInRange :: Date -> Date -> IO [Room]
freeInRange checkIn checkOut =
    if checkOut <= checkIn
    then pure []
    else do
        roomsByType <- getRooms
        reservations <- getReservations
        let rooms = concat roomsByType
            busy = map fst (bookedPairs reservations (overlapsRange checkIn checkOut))
        pure (filter (\room -> roomId room `notElem` busy) rooms)