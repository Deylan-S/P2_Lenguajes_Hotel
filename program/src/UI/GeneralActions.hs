module UI.GeneralActions
    ( actionViewAvailability
    , actionMakeReservation
    , actionCancelReservation
    , actionInvoiceReservation
    ) where

import Data.List (sortOn)

import Availability (availableRooms, freeInRange)
import Billing (invoiceReservation, printInvoice)
import Logic.BookingValidation
import RatesDM (getRates)
import ReservationDM (getReservations)
import Reservations (cancelReservation, makeReservation, printReceipt)
import RoomDM (getRooms)
import RoomTypeDM (getRoomTypes)
import Types
import UI.Common (withGeneratedRooms)
import UI.Display (printError, printLines, printPanel, printSuccess)
import UI.Formatters
import UI.Input
import Utils.DateUtils (formatDateWithWeekday, getCurrentDateTime)

-- ---------------------------------------------------------------
-- Ver disponibilidad
-- ---------------------------------------------------------------

-- Objetivo: opcion general para ver las habitaciones disponibles en un dia
-- Entradas: pide la fecha al usuario
-- Salidas: imprime las habitaciones disponibles
-- Restricciones: requiere habitaciones generadas
actionViewAvailability :: IO ()
actionViewAvailability = do
    printPanel "VER DISPONIBILIDAD"
    withGeneratedRooms (\_ -> do
        date <- readValidDate "Día a consultar (dd/mm/aaaa): "
        freeRooms <- availableRooms date
        putStrLn ("\nHabitaciones disponibles el " ++ formatDateWithWeekday date
                  ++ ": " ++ show (length freeRooms))
        printLines (tableOrNone ["ID", "Tipo"] (map roomRow (sortOn roomId freeRooms))))

-- ---------------------------------------------------------------
-- Reservacion
-- ---------------------------------------------------------------

-- Objetivo: opcion general para crear una reservacion
-- Entradas: pide los datos al usuario
-- Salidas: imprime el comprobante si se registra
-- Restricciones: requiere habitaciones generadas y tarifas cargadas
actionMakeReservation :: IO ()
actionMakeReservation = do
    printPanel "NUEVA RESERVACIÓN"
    allRooms <- concat <$> getRooms
    rates <- getRates
    case checkReservationReady allRooms rates of
        Just problem -> printError problem
        Nothing      -> getRoomTypes >>= requestReservation

-- Objetivo: pedir los datos de la reservacion y enviarlos al modulo de reservaciones
-- Entradas: tipos de habitacion registrados
-- Salidas: ninguna
-- Restricciones: si no hay habitaciones libres en las fechas, termina con un aviso
requestReservation :: [RoomType] -> IO ()
requestReservation roomTypes = do
    guestName <- readPersonName "Nombre de quien reserva: "
    (checkIn, checkOut) <- readStayDates
    freeRooms <- freeInRange checkIn checkOut
    if null freeRooms
        then printError "No hay habitaciones disponibles para esas fechas."
        else do
            requests <- collectRoomRequests roomTypes freeRooms []
            submitReservation guestName checkIn checkOut requests

-- Objetivo: pedir las fechas de ingreso y salida hasta que sean validas
-- Entradas: ninguna
-- Salidas: (fecha de ingreso, fecha de salida)
-- Restricciones: la salida debe ser posterior al ingreso
readStayDates :: IO (Date, Date)
readStayDates = do
    checkIn <- readValidDate "Fecha de ingreso (dd/mm/aaaa): "
    checkOut <- readValidDate "Fecha de salida (dd/mm/aaaa): "
    case validateStay checkIn checkOut of
        Left problem -> do
            printError problem
            readStayDates
        Right () -> pure (checkIn, checkOut)

-- Objetivo: pedir las habitaciones de la reservacion una por una
-- Entradas: tipos, habitaciones libres para las fechas y solicitudes ya elegidas
-- Salidas: lista de solicitudes (tipo, adultos, ninos)
-- Restricciones: no permite pedir mas habitaciones de las disponibles por tipo
collectRoomRequests :: [RoomType] -> [Room] -> [RoomRequest] -> IO [RoomRequest]
collectRoomRequests roomTypes freeRooms chosen =
    case remainingTypeOptions roomTypes freeRooms chosen of
        [] -> do
            putStrLn "\nNo quedan más habitaciones disponibles para esas fechas."
            pure chosen
        options -> do
            newRequest <- askRoomRequest options
            askForMore roomTypes freeRooms (chosen ++ [newRequest])

-- Objetivo: preguntar si se desea agregar otra habitacion y continuar si es asi
-- Entradas: tipos, habitaciones libres y solicitudes elegidas hasta ahora
-- Salidas: lista final de solicitudes
-- Restricciones: no pregunta si ya no quedan habitaciones por elegir
askForMore :: [RoomType] -> [Room] -> [RoomRequest] -> IO [RoomRequest]
askForMore roomTypes freeRooms chosen
    | null (remainingTypeOptions roomTypes freeRooms chosen) = pure chosen
    | otherwise = do
        more <- readYesNo "\n¿Desea agregar otra habitación? (s/n): "
        if more
            then collectRoomRequests roomTypes freeRooms chosen
            else pure chosen

-- Objetivo: pedir tipo y huespedes de una habitacion
-- Entradas: tipos con habitaciones aun disponibles
-- Salidas: solicitud (tipo, adultos, ninos)
-- Restricciones: los huespedes no pueden superar el maximo del tipo
askRoomRequest :: [(RoomType, Int)] -> IO RoomRequest
askRoomRequest options = do
    putStrLn "\nTipos de habitación disponibles:"
    printLines (formatTable ["#", "Tipo", "Máx. huéspedes", "Disponibles"]
                            (zipWith typeOptionRow [1 ..] options))
    choice <- readIntInRange "Seleccione el tipo (número): " 1 (length options)
    let selectedType = fst (options !! (choice - 1))
    (adults, children) <- readGuestCounts (rtMaxGuests selectedType)
    pure (rtName selectedType, adults, children)

-- Objetivo: pedir la cantidad de adultos y ninos de una habitacion
-- Entradas: maximo de huespedes del tipo de habitacion
-- Salidas: (adultos, ninos)
-- Restricciones: al menos un huesped y sin superar el maximo
readGuestCounts :: Int -> IO (Int, Int)
readGuestCounts maxGuests = do
    adults <- readIntInRange ("Cantidad de adultos (0 a " ++ show maxGuests ++ "): ") 0 maxGuests
    children <- readChildrenCount (maxGuests - adults)
    if adults + children < 1
        then do
            printError "La habitación debe tener al menos un huésped."
            readGuestCounts maxGuests
        else pure (adults, children)

-- Objetivo: pedir la cantidad de ninos segun el espacio restante
-- Entradas: espacios restantes en la habitacion
-- Salidas: cantidad de ninos
-- Restricciones: si no queda espacio no pregunta y devuelve 0
readChildrenCount :: Int -> IO Int
readChildrenCount 0 = pure 0
readChildrenCount remaining =
    readIntInRange ("Cantidad de niños (0 a " ++ show remaining ++ "): ") 0 remaining

-- Objetivo: validar la solicitud, mostrar el resumen y pedir confirmacion
-- Entradas: nombre, fecha de ingreso, fecha de salida y habitaciones solicitadas
-- Salidas: ninguna (registra la reservacion si el usuario confirma)
-- Restricciones: si la solicitud no es valida muestra el motivo y no registra nada
submitReservation :: String -> Date -> Date -> [RoomRequest] -> IO ()
submitReservation guestName checkIn checkOut requests =
    case validateBooking checkIn checkOut requests of
        Left problem -> printError problem
        Right () -> do
            putStrLn ""
            printLines (bookingSummaryLines guestName checkIn checkOut requests)
            confirmed <- readYesNo "\n¿Confirmar la reservación? (s/n): "
            if confirmed
                then registerReservation guestName checkIn checkOut requests
                else putStrLn "Reservación descartada. No se guardó ningún dato."

-- Objetivo: registrar la reservacion con el modulo de reservaciones y mostrar el comprobante
-- Entradas: nombre, fecha de ingreso, fecha de salida y habitaciones solicitadas
-- Salidas: imprime el comprobante o el motivo del rechazo
-- Restricciones: la fecha y hora de la reservacion es la actual del sistema
registerReservation :: String -> Date -> Date -> [RoomRequest] -> IO ()
registerReservation guestName checkIn checkOut requests = do
    now <- getCurrentDateTime
    result <- makeReservation guestName now checkIn checkOut requests
    case result of
        Left problem -> printError problem
        Right reservation -> do
            printSuccess "Reservación registrada. Comprobante:"
            putStrLn ""
            printReceipt reservation

-- ---------------------------------------------------------------
-- Anular y facturar reservacion
-- ---------------------------------------------------------------

-- Objetivo: opcion general para anular una reservacion activa
-- Entradas: pide el codigo al usuario
-- Salidas: imprime el resultado
-- Restricciones: la reservacion debe existir y estar activa
actionCancelReservation :: IO ()
actionCancelReservation = do
    printPanel "ANULAR RESERVACIÓN"
    confirmAndRun "\n¿Desea cancelar esta reservación? (s/n): " cancelOperation

-- Objetivo: opcion general para facturar una reservacion activa
-- Entradas: pide el codigo al usuario
-- Salidas: imprime la factura
-- Restricciones: la reservacion debe existir y estar activa
actionInvoiceReservation :: IO ()
actionInvoiceReservation = do
    printPanel "FACTURAR RESERVACIÓN"
    confirmAndRun "\n¿Desea facturar esta reservación? (s/n): " invoiceOperation

-- Objetivo: pedir un codigo de reservacion, mostrar su detalle, confirmar y ejecutar una operacion
-- Entradas: la pregunta de confirmacion y la operacion a ejecutar con el codigo
-- Salidas: ninguna
-- Restricciones: si la reservacion no existe o no esta activa se ejecuta la operacion sin
--                confirmar, para que el modulo de reservaciones informe el motivo
confirmAndRun :: String -> (Int -> IO ()) -> IO ()
confirmAndRun question operation = do
    code <- readPositiveInt "Código de la reservación: "
    reservations <- getReservations
    case findReservation code reservations of
        Just reservation | resStatus reservation == Active -> do
            putStrLn ""
            printLines (reservationEntryLines reservation)
            confirmed <- readYesNo question
            if confirmed
                then operation code
                else putStrLn "Operación cancelada."
        _ -> operation code

-- Objetivo: anular una reservacion y mostrar el resultado
-- Entradas: codigo de la reservacion
-- Salidas: imprime el resultado
-- Restricciones: ver cancelReservation
cancelOperation :: Int -> IO ()
cancelOperation code = do
    result <- cancelReservation code
    case result of
        Left problem -> printError problem
        Right ()     -> printSuccess "Reservación cancelada. Las habitaciones quedaron libres."

-- Objetivo: facturar una reservacion y mostrar la factura
-- Entradas: codigo de la reservacion
-- Salidas: imprime la factura o el motivo del rechazo
-- Restricciones: ver invoiceReservation
invoiceOperation :: Int -> IO ()
invoiceOperation code = do
    result <- invoiceReservation code
    case result of
        Left problem -> printError problem
        Right invoice -> do
            printSuccess "Reservación facturada. Factura:"
            putStrLn ""
            printInvoice invoice
