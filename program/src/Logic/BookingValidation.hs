module Logic.BookingValidation
    ( maxStayNights
    , checkReservationReady
    , validateStay
    , validateRequests
    , validateBooking
    , countRoomsOfType
    , remainingTypeOptions
    , findReservation
    ) where

import Data.List (find)

import Logic.SetupLogic (isValidRateSet)
import Types
import Utils.DateUtils (nightsBetween)

-- Objetivo: cantidad maxima de noches de una estadia
-- Entradas: ninguna
-- Salidas: el limite
-- Restricciones: evita estadias enormes por errores de digitacion
maxStayNights :: Int
maxStayNights = 365

-- Objetivo: verificar que el sistema este listo para reservar antes de pedir los datos
-- Entradas: habitaciones generadas y tarifas cargadas
-- Salidas: Just mensaje si falta algo, Nothing si todo esta listo
-- Restricciones: se necesitan habitaciones generadas y las 8 tarifas
checkReservationReady :: [Room] -> [Rate] -> Maybe String
checkReservationReady rooms rates
    | null rooms                 = Just "Aún no se han generado las habitaciones del hotel."
    | not (isValidRateSet rates) = Just "Aún no se han cargado las tarifas."
    | otherwise                  = Nothing

-- Objetivo: validar las fechas de una estadia
-- Entradas: fecha de ingreso y fecha de salida
-- Salidas: Right () si son validas, Left mensaje si no
-- Restricciones: la salida debe ser posterior al ingreso y no superar el maximo de noches
validateStay :: Date -> Date -> Either String ()
validateStay checkIn checkOut
    | checkOut <= checkIn =
        Left "La fecha de salida debe ser posterior a la fecha de ingreso."
    | nightsBetween checkIn checkOut > maxStayNights =
        Left ("La estadía no puede superar " ++ show maxStayNights ++ " noches.")
    | otherwise = Right ()

-- Objetivo: validar las habitaciones solicitadas
-- Entradas: lista de solicitudes (tipo, adultos, ninos)
-- Salidas: Right () si son validas, Left mensaje si no
-- Restricciones: al menos una habitacion y al menos un huesped por habitacion
validateRequests :: [RoomRequest] -> Either String ()
validateRequests requests
    | null requests =
        Left "Debe solicitar al menos una habitación."
    | any (not . hasGuests) requests =
        Left "Cada habitación debe tener al menos un huésped."
    | otherwise = Right ()
  where
    hasGuests (_, adults, children) = adults >= 0 && children >= 0 && adults + children >= 1

-- Objetivo: validar una solicitud completa antes de enviarla al modulo de reservaciones
-- Entradas: fecha de ingreso, fecha de salida y las habitaciones solicitadas
-- Salidas: Right () si todo es valido, Left con el primer problema encontrado
-- Restricciones: ver validateStay y validateRequests
validateBooking :: Date -> Date -> [RoomRequest] -> Either String ()
validateBooking checkIn checkOut requests =
    validateStay checkIn checkOut >> validateRequests requests

-- Objetivo: contar las habitaciones de un tipo en una lista
-- Entradas: nombre del tipo y lista de habitaciones
-- Salidas: cantidad de habitaciones de ese tipo
-- Restricciones: ninguna
countRoomsOfType :: String -> [Room] -> Int
countRoomsOfType typeName rooms = length (filter ((== typeName) . roomType) rooms)

-- Objetivo: calcular los tipos que aun tienen habitaciones por elegir
-- Entradas: tipos registrados, habitaciones libres y solicitudes ya elegidas
-- Salidas: lista de (tipo, cantidad restante) solo con cantidades mayores a cero
-- Restricciones: la cantidad restante descuenta las solicitudes ya elegidas
remainingTypeOptions :: [RoomType] -> [Room] -> [RoomRequest] -> [(RoomType, Int)]
remainingTypeOptions roomTypes freeRooms chosen =
    [ (candidate, remaining)
    | candidate <- roomTypes
    , let remaining = countRoomsOfType (rtName candidate) freeRooms - requestedOfType (rtName candidate)
    , remaining > 0
    ]
  where
    requestedOfType name = length [ () | (typeName, _, _) <- chosen, typeName == name ]

-- Objetivo: buscar una reservacion por su codigo
-- Entradas: codigo y lista de reservaciones
-- Salidas: Just reservacion si existe, Nothing si no
-- Restricciones: ninguna
findReservation :: Int -> [Reservation] -> Maybe Reservation
findReservation code = find ((== code) . resId)
