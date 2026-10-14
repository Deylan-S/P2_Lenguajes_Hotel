module Reservations
    ( makeReservation
    , cancelReservation
    , formatReceipt
    , printReceipt
    , reservationDetailLines
    , formatMoney
    ) where

import Availability (freeInRange)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, find, intercalate, nub)
import Dates (formatDate, isValidDate)
import Numeric (showFFloat)
import Rates (stayCost)
import RatesDM (getRates)
import ReservationDM (addReservation, changeReservationStatus, getReservations)
import RoomDM (getRooms)
import RoomTypeDM (getRoomTypes)
import Types

-- Objetivo: crear una reservación, guardarla en la base de datos y devolverla
-- Entradas: nombre de la persona que reserva, fecha y hora de la reservación, fecha de ingreso, fecha de salida
--           y una lista de habitaciones pedidas
-- Salidas: Right con la reservación creada (estado Active, monto sin impuesto),
--          o Left con el mensaje del primer error encontrado
-- Restricciones: cada habitación no puede superar el máximo de ocupantes de su tipo, 
--                debe haber habitaciones libres de cada tipo en todo el rango de fechas, 
--                y deben estar generadas las habitaciones y cargadas las 8 tarifas
makeReservation :: String -> DateTime -> Date -> Date -> [RoomRequest] -> IO (Either String Reservation)
makeReservation guestName createdAt checkIn checkOut requests = do
    roomTypes <- getRoomTypes
    rooms <- getRooms
    rates <- getRates
    freeRooms <- freeInRange checkIn checkOut
    case planReservation roomTypes (concat rooms) rates freeRooms checkIn checkOut requests of
        Left message -> pure (Left message)
        Right (occupancies, total) -> do
            reservations <- getReservations
            let reservation = Reservation
                    { resId          = nextReservationId reservations
                    , resGuestName   = trim guestName
                    , resCreatedAt   = createdAt
                    , resCheckIn     = checkIn
                    , resCheckOut    = checkOut
                    , resAdults      = sum (map occAdults occupancies)
                    , resChildren    = sum (map occChildren occupancies)
                    , resStatus      = Active
                    , resOccupancies = occupancies
                    , resTotal       = total
                    }
            addReservation reservation
            pure (Right reservation)

-- Objetivo: validar los datos guardados, elegir las habitaciones y calcular el monto
-- Entradas: los tipos de habitación, todas las habitaciones, tarifas, habitaciones libres en el rango,
--           fecha de ingreso, fecha de salida y habitaciones pedidas
-- Salidas: Right con las ocupaciones (habitación asignada, tipo, adultos, niños) y el monto total sin impuesto,
--          o Left con el mensaje del primer error
-- Restricciones: ninguna
planReservation :: [RoomType] -> [Room] -> [Rate] -> [Room] -> Date -> Date -> [RoomRequest]
                -> Either String ([RoomOccupancy], Double)
planReservation roomTypes allRooms rates freeRooms checkIn checkOut requests = do
    check (not (null allRooms)) "Aún no se han generado las habitaciones del hotel"
    check (all (\i -> any (\r -> rateId r == i) rates) [1 .. 8]) "Aún no se han cargado las tarifas"
    resolved <- mapM (resolveType roomTypes) requests
    mapM_ (uncurry checkCapacity) (zip [1 ..] resolved)
    checkAvailability freeRooms resolved
    occupancies <- maybe (Left "No fue posible asignar las habitaciones solicitadas") Right
                         (assignRooms freeRooms resolved)
    costs <- maybe (Left "No fue posible calcular el monto: falta alguna tarifa") Right
                   (mapM (\o -> stayCost rates checkIn checkOut (occAdults o) (occChildren o)) occupancies)
    Right (occupancies, sum costs)

-- Objetivo: convertir una condición en un resultado Either
-- Entradas: una condición y el mensaje de error
-- Salidas: Right () si la condición es verdadera, Left con el mensaje si es falsa
-- Restricciones: ninguna
check :: Bool -> String -> Either String ()
check condition message =
    if condition
    then Right ()
    else Left message

-- Objetivo: buscar el tipo de habitación que escribió el usuario
-- Entradas: los tipos guardados y una habitación pedida (tipo, adultos, niños)
-- Salidas: Right con el tipo encontrado y las cantidades, o Left si el tipo no existe
-- Restricciones: ninguna
resolveType :: [RoomType] -> RoomRequest -> Either String (RoomType, Int, Int)
resolveType roomTypes (typeName, adults, children) =
    case find (\rt -> map toLower (rtName rt) == map toLower (trim typeName)) roomTypes of
        Just rt -> Right (rt, adults, children)
        Nothing -> Left ("El tipo de habitación \"" ++ typeName ++ "\" no existe.")

-- Objetivo: verificar que una habitación no supere el máximo de huéspedes de su tipo
-- Entradas: el número de la habitación dentro de la solicitud y (tipo, adultos, niños)
-- Salidas: Right () si cabe, Left con el mensaje si no
-- Restricciones: ninguna
checkCapacity :: Int -> (RoomType, Int, Int) -> Either String ()
checkCapacity index (rt, adults, children) =
    check (adults + children <= rtMaxGuests rt)
          ("La habitación " ++ show index ++ " (tipo " ++ rtName rt ++ ") admite máximo "
              ++ show (rtMaxGuests rt) ++ " huéspedes y se indicaron " ++ show (adults + children))

-- Objetivo: verificar que haya suficientes habitaciones libres de cada tipo pedido
-- Entradas: las habitaciones libres en todo el rango y las habitaciones pedidas ya resueltas
-- Salidas: Right () si alcanzan, Left con el mensaje del primer tipo que no alcanza
-- Restricciones: ninguna
checkAvailability :: [Room] -> [(RoomType, Int, Int)] -> Either String ()
checkAvailability freeRooms resolved = mapM_ checkType (nub (map (\(rt, _, _) -> rtName rt) resolved))
  where
    checkType name =
        let requested = length (filter (\(rt, _, _) -> rtName rt == name) resolved)
            available = length (filter (\room -> roomType room == name) freeRooms)
        in check (requested <= available)
                 ("No hay disponibilidad: se solicitaron " ++ show requested ++ " habitación(es) de tipo "
                     ++ name ++ " y solo hay " ++ show available ++ " disponible(s) para esas fechas")

-- Objetivo: asignar una habitación libre diferente a cada habitación pedida
-- Entradas: las habitaciones libres y las habitaciones pedidas ya resueltas
-- Salidas: las ocupaciones en el mismo orden de la solicitud, o Nothing si falta alguna habitación
-- Restricciones: una habitación libre no se asigna dos veces
assignRooms :: [Room] -> [(RoomType, Int, Int)] -> Maybe [RoomOccupancy]
assignRooms _ [] = Just []
assignRooms freeRooms ((rt, adults, children) : rest) =
    case break (\room -> roomType room == rtName rt) freeRooms of
        (before, room : after) ->
            fmap (RoomOccupancy (roomId room) (rtName rt) adults children :) (assignRooms (before ++ after) rest)
        (_, []) -> Nothing

-- Objetivo: calcular el identificador de la próxima reservación
-- Entradas: las reservaciones guardadas
-- Salidas: 1 si no hay ninguna, o el mayor identificador más 1
-- Restricciones: ninguna
nextReservationId :: [Reservation] -> Int
nextReservationId reservations =
    if null reservations
    then 1
    else maximum (map resId reservations) + 1

-- Anulación ------

-- Objetivo: anular una reservación activa y liberar sus habitaciones para esas fechas
-- Entradas: el código de la reservación
-- Salidas: Right () si se anuló, o Left con el mensaje si no existe, ya está anulada o ya fue facturada
-- Restricciones: solo se pueden anular reservaciones en estado Active (si queda Cancelled, la
--                disponibilidad deja de contarla, por lo que las habitaciones quedan libres)
cancelReservation :: Int -> IO (Either String ())
cancelReservation targetId = do
    reservations <- getReservations
    case find (\r -> resId r == targetId) reservations of
        Nothing -> pure (Left ("No existe una reservación con el código " ++ show targetId))
        Just reservation
            | resStatus reservation == Active -> do
                changeReservationStatus targetId Cancelled
                pure (Right ())
            | resStatus reservation == Cancelled ->
                pure (Left ("La reservación " ++ show targetId ++ " ya está anulada"))
            | otherwise ->
                pure (Left ("La reservación " ++ show targetId ++ " ya fue facturada y no se puede anulars"))

-- Objetivo: armar el texto del comprobante de una reservación
-- Entradas: una reservación
-- Salidas: el comprobante
-- Restricciones: ninguna
formatReceipt :: Reservation -> String
formatReceipt reservation = intercalate "\n" $
    [ "========== COMPROBANTE DE RESERVA ==========" ]
    ++ reservationDetailLines reservation
    ++ [ "============================================" ]

-- Objetivo: mostrar el comprobante en pantalla
-- Entradas: una reservación
-- Salidas: ninguna (imprime el comprobante)
-- Restricciones: no se exporta ni se guarda en archivos, solo se muestra
printReceipt :: Reservation -> IO ()
printReceipt reservation = putStrLn (formatReceipt reservation)

-- Objetivo: armar las líneas con el detalle de una reservación
-- Entradas: una reservación
-- Salidas: lista de líneas con identificador, nombre, fecha y hora, fechas, huéspedes, total y habitaciones
-- Restricciones: el total mostrado es sin impuesto
reservationDetailLines :: Reservation -> [String]
reservationDetailLines r =
    [ "Identificador de reserva:   " ++ show (resId r)
    , "Nombre de quien reserva:    " ++ resGuestName r
    , "Fecha y hora de reserva:    " ++ formatDateTime (resCreatedAt r)
    , "Fecha de ingreso:           " ++ formatDate (resCheckIn r)
    , "Fecha de salida:            " ++ formatDate (resCheckOut r)
    , "Cantidad de adultos:        " ++ show (resAdults r)
    , "Cantidad de niños:          " ++ show (resChildren r)
    , "Total de la reserva:        " ++ formatMoney (resTotal r) ++ " (sin impuestos)"
    , "Habitaciones reservadas:"
    ] ++ map roomLine (resOccupancies r)
  where
    roomLine o = "  - Habitación " ++ show (occRoomId o) ++ " | Tipo: " ++ occRoomType o
                    ++ " | Adultos: " ++ show (occAdults o) ++ " | Niños: " ++ show (occChildren o)

-- Objetivo: dar formato de dinero a un monto
-- Entradas: un monto
-- Salidas: texto con dos decimales
-- Restricciones: ninguna
formatMoney :: Double -> String
formatMoney amount = showFFloat (Just 2) amount ""

-- Objetivo: dar formato a una fecha y hora para mostrarla en pantalla
-- Entradas: una fecha y hora
-- Salidas: texto con el formato dd/mm/aaaa hh:mm
-- Restricciones: ninguna
formatDateTime :: DateTime -> String
formatDateTime (DateTime date hour minute) = formatDate date ++ " " ++ padTwo hour ++ ":" ++ padTwo minute
  where
    padTwo n = if n < 10 then '0' : show n else show n

-- Objetivo: quitar los espacios al inicio y al final de un texto
-- Entradas: un texto
-- Salidas: el texto sin espacios en los extremos
-- Restricciones: ninguna
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace