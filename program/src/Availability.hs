module Availability (availableRooms, occupiedRooms, freeInRange) where

import Types

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
-- Entradas: el sistema y una condición sobre reservas
-- Salidas:  lista de datos (id de habitación y id de reserva)
-- Restricciones: ignora las reservas canceladas
bookedPairs :: System -> (Reservation -> Bool) -> [(Int, Int)]
bookedPairs sys covers =
    let reservasFiltradas = filter (\r -> if blocksRooms r && covers r then True else False) (sysReservations sys)
    in concatMap pairsOf reservasFiltradas
  where
    pairsOf r = map (\o -> (occRoomId o, resId r)) (resOccupancies r)

-- funciones que se deben exportar

-- Objetivo: listar las habitaciones libres en un día específico
-- Entradas: el estado del sistema y la fecha a consultar
-- Salidas: lista de habitaciones (Room) disponibles ese día
-- Restricciones: si no se han generado habitaciones, devuelve una lista vacía
availableRooms :: System -> Date -> [Room]
availableRooms sys date = filter (\room -> if elem (roomId room) busy then False else True) (sysRooms sys)
  where
    busy = map (\pair -> fst pair) (bookedPairs sys (coversDate date))

-- Objetivo: listar las habitaciones ocupadas en un día y la reserva que las ocupa
-- Entradas: el estado del sistema y la fecha a consultar
-- Salidas: lista de datos (habitación, código de reserva)
-- Restricciones: las reservas canceladas no cuentan, las facturadas sí
occupiedRooms :: System -> Date -> [(Room, Int)]
occupiedRooms sys date = concatMap withReservation (sysRooms sys)
  where
    booked = bookedPairs sys (coversDate date)
    withReservation room = [ (room, rid) | (rmId, rid) <- booked, rmId == roomId room ]

-- Objetivo: listar las habitaciones libres durante todo un rango de noches
-- Entradas: el sistema, la fecha de entrada y la fecha de salida
-- Salidas: lista de habitaciones libres cada noche del rango
-- Restricciones: la entrada debe ser menor que la salida, sino devuelve una lista vacía
freeInRange :: System -> Date -> Date -> [Room]
freeInRange sys checkIn checkOut =
    if checkOut <= checkIn
    then []
    else filter (\room -> if elem (roomId room) busy then False else True) (sysRooms sys)
  where
    busy = map (\pair -> fst pair) (bookedPairs sys (overlapsRange checkIn checkOut))