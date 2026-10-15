module UI.Formatters
    ( formatTable
    , tableOrNone
    , statusName
    , roomRow
    , occupiedRoomRow
    , rateRow
    , roomTypeRow
    , typeOptionRow
    , planRow
    , guestsRow
    , weekdayRow
    , reservationEntryLines
    , bookingSummaryLines
    , occupancyReportLines
    ) where

import Data.List (intercalate, sortOn, transpose)

import Reservations (formatMoney, reservationDetailLines)
import Types
import Utils.DateUtils (formatDateWithWeekday, nightsBetween, weekdayName)
import Utils.TextUtils (pad2, padRight)

-- Objetivo: construir una tabla de texto con columnas alineadas
-- Entradas: encabezados y filas
-- Salidas: lineas de texto de la tabla
-- Restricciones: ninguna
formatTable :: [String] -> [[String]] -> [String]
formatTable headers rows =
    [formatTableRow widths headers, separatorLine widths] ++ map (formatTableRow widths) rows
  where
    widths = map (maximum . map length) (transpose (headers : rows))

-- Objetivo: dar formato a una fila de la tabla
-- Entradas: anchos de columna y celdas de la fila
-- Salidas: la fila como texto
-- Restricciones: ninguna
formatTableRow :: [Int] -> [String] -> String
formatTableRow widths cells = intercalate " | " (zipWith padRight widths cells)

-- Objetivo: construir la linea que separa los encabezados de las filas
-- Entradas: anchos de columna
-- Salidas: linea de guiones
-- Restricciones: ninguna
separatorLine :: [Int] -> String
separatorLine widths = intercalate "-+-" (map (\w -> replicate w '-') widths)

-- Objetivo: construir una tabla o un texto indicando que no hay datos
-- Entradas: encabezados y filas
-- Salidas: lineas de la tabla, o "(ninguna)" si no hay filas
-- Restricciones: ninguna
tableOrNone :: [String] -> [[String]] -> [String]
tableOrNone _ [] = ["  (ninguna)"]
tableOrNone headers rows = formatTable headers rows

-- Objetivo: obtener el nombre de un estado de reservacion
-- Entradas: el estado
-- Salidas: nombre del estado
-- Restricciones: ninguna
statusName :: ReservationStatus -> String
statusName Active    = "Activa"
statusName Invoiced  = "Facturada"
statusName Cancelled = "Cancelada"

-- Objetivo: construir la fila de una habitacion
-- Entradas: una habitacion
-- Salidas: [id, tipo]
-- Restricciones: ninguna
roomRow :: Room -> [String]
roomRow room = [show (roomId room), roomType room]

-- Objetivo: construir la fila de una habitacion ocupada
-- Entradas: habitacion y codigo de la reservacion que la ocupa
-- Salidas: [id, tipo, codigo de reserva]
-- Restricciones: ninguna
occupiedRoomRow :: (Room, Int) -> [String]
occupiedRoomRow (room, code) = roomRow room ++ [show code]

-- Objetivo: obtener la descripcion de una tarifa segun su id
-- Entradas: id de la tarifa (1 a 8)
-- Salidas: descripcion de la tarifa
-- Restricciones: devuelve "Tarifa" si el id no existe
rateDescription :: Int -> String
rateDescription wantedId =
    case lookup wantedId descriptions of
        Just description -> description
        Nothing          -> "Tarifa"
  where
    descriptions =
        [ (1, "Verde entre semana adulto (dom-jue, abr-oct)")
        , (2, "Verde entre semana niño (dom-jue, abr-oct)")
        , (3, "Verde fin de semana adulto (vie-sáb, abr-oct)")
        , (4, "Verde fin de semana niño (vie-sáb, abr-oct)")
        , (5, "Alta entre semana adulto (dom-jue, nov-mar)")
        , (6, "Alta entre semana niño (dom-jue, nov-mar)")
        , (7, "Alta fin de semana adulto (vie-sáb, nov-mar)")
        , (8, "Alta fin de semana niño (vie-sáb, nov-mar)")
        ]

-- Objetivo: construir la fila de una tarifa
-- Entradas: una tarifa
-- Salidas: [id, descripcion, monto]
-- Restricciones: ninguna
rateRow :: Rate -> [String]
rateRow rate = [show (rateId rate), rateDescription (rateId rate), formatMoney (rateAmount rate)]

-- Objetivo: construir la fila de un tipo de habitacion
-- Entradas: un tipo de habitacion
-- Salidas: [nombre, descripcion, maximo de huespedes]
-- Restricciones: ninguna
roomTypeRow :: RoomType -> [String]
roomTypeRow roomTypeValue =
    [rtName roomTypeValue, rtDescription roomTypeValue, show (rtMaxGuests roomTypeValue)]

-- Objetivo: construir la fila de una opcion de tipo de habitacion
-- Entradas: numero de opcion y (tipo, habitaciones disponibles)
-- Salidas: [numero, nombre, maximo de huespedes, disponibles]
-- Restricciones: ninguna
typeOptionRow :: Int -> (RoomType, Int) -> [String]
typeOptionRow number (roomTypeValue, available) =
    [show number, rtName roomTypeValue, show (rtMaxGuests roomTypeValue), show available]

-- Objetivo: construir la fila de un renglon del plan de habitaciones
-- Entradas: (nombre del tipo, cantidad)
-- Salidas: [tipo, cantidad]
-- Restricciones: ninguna
planRow :: (String, Int) -> [String]
planRow (typeName, quantity) = [typeName, show quantity]

-- Objetivo: construir la fila de huespedes por mes y anio
-- Entradas: ((anio, mes), huespedes)
-- Salidas: [mes-anio, huespedes]
-- Restricciones: ninguna
guestsRow :: ((Int, Int), Int) -> [String]
guestsRow ((y, m), guests) = [pad2 m ++ "-" ++ show y, show guests]

-- Objetivo: construir la fila de un dia de la semana con su ocupacion
-- Entradas: posicion en el ranking y (dia de la semana, habitaciones ocupadas)
-- Salidas: [posicion, nombre del dia, ocupaciones]
-- Restricciones: ninguna
weekdayRow :: Int -> (Int, Int) -> [String]
weekdayRow position (weekday, occupied) = [show position, weekdayName weekday, show occupied]

-- Objetivo: construir el detalle de una reservacion incluyendo su estado
-- Entradas: una reservacion
-- Salidas: lineas de texto con el detalle del modulo de reservaciones y el estado
-- Restricciones: el estado se coloca justo despues del identificador
reservationEntryLines :: Reservation -> [String]
reservationEntryLines reservation =
    replicate 44 '-' : insertStatus (reservationDetailLines reservation)
  where
    statusLine = "Estado de la reserva:       " ++ statusName (resStatus reservation)
    insertStatus (idLine : otherLines) = idLine : statusLine : otherLines
    insertStatus []                    = [statusLine]

-- Objetivo: construir el resumen de una solicitud de reservacion antes de confirmarla
-- Entradas: nombre, fecha de ingreso, fecha de salida y habitaciones solicitadas
-- Salidas: lineas de texto con el resumen
-- Restricciones: ninguna
bookingSummaryLines :: String -> Date -> Date -> [RoomRequest] -> [String]
bookingSummaryLines guestName checkIn checkOut requests =
    [ "Resumen de la solicitud:"
    , "  Nombre de quien reserva : " ++ guestName
    , "  Fecha de ingreso        : " ++ formatDateWithWeekday checkIn
    , "  Fecha de salida         : " ++ formatDateWithWeekday checkOut
    , "  Noches                  : " ++ show (nightsBetween checkIn checkOut)
    , ""
    ]
    ++ formatTable ["Habitación", "Tipo", "Adultos", "Niños"] (zipWith requestRow [1 :: Int ..] requests)
  where
    requestRow number (typeName, adults, children) =
        [show number, typeName, show adults, show children]

-- Objetivo: construir el reporte de ocupacion de un dia
-- Entradas: fecha, habitaciones disponibles y habitaciones ocupadas con su reservacion
-- Salidas: lineas de texto con ambas tablas
-- Restricciones: ninguna
occupancyReportLines :: Date -> [Room] -> [(Room, Int)] -> [String]
occupancyReportLines date freeRooms busyRooms =
    [ "Estado de ocupación para el " ++ formatDateWithWeekday date
    , ""
    , "Habitaciones disponibles (" ++ show (length freeRooms) ++ "):"
    ]
    ++ tableOrNone ["ID", "Tipo"] (map roomRow (sortOn roomId freeRooms))
    ++ [ ""
       , "Habitaciones ocupadas (" ++ show (length busyRooms) ++ "):"
       ]
    ++ tableOrNone ["ID", "Tipo", "Reserva"] (map occupiedRoomRow (sortOn (roomId . fst) busyRooms))
