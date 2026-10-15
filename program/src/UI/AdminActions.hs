module UI.AdminActions
    ( actionLoadRoomTypes
    , actionAssignRooms
    , actionViewRooms
    , actionLoadRates
    , actionConsultReservations
    , actionConsultOccupancy
    ) where

import Data.List (sortOn)
import System.Directory (doesFileExist)

import Availability (availableRooms, occupiedRooms)
import Logic.SetupLogic
import RatesDM (getRatesFromTxt, loadRates)
import ReservationDM (getReservations)
import RoomDM (getRooms, populateRoomsOfType)
import RoomTypeDM (addRoomType, getRoomTypes, getRtFromTxt)
import Types
import UI.Common (withGeneratedRooms)
import UI.Display (printError, printLines, printPanel, printSuccess)
import UI.Formatters
import UI.Input
import Utils.TextUtils (isBlank)

-- ---------------------------------------------------------------
-- Cargar tipos de habitaciones
-- ---------------------------------------------------------------

-- Objetivo: opcion administrativa para cargar tipos de habitacion desde un archivo
-- Entradas: pide la ruta al usuario
-- Salidas: imprime el resultado
-- Restricciones: no se permiten tipos con el mismo nombre, se agregan solo los nuevos
actionLoadRoomTypes :: IO ()
actionLoadRoomTypes = do
    printPanel "CARGAR TIPOS DE HABITACIÓN"
    maybePath <- readOptionalPath "Ruta del archivo de tipos (enter para cancelar): "
    case maybePath of
        Nothing   -> putStrLn "Operación cancelada."
        Just path -> loadRoomTypesFromFile path

-- Objetivo: cargar tipos de habitacion si el archivo existe
-- Entradas: ruta del archivo
-- Salidas: imprime el resultado
-- Restricciones: muestra un error si el archivo no existe
loadRoomTypesFromFile :: FilePath -> IO ()
loadRoomTypesFromFile path = do
    exists <- doesFileExist path
    if exists
        then importRoomTypes path
        else printError "El archivo indicado no existe."

-- Objetivo: leer, validar y registrar los tipos de un archivo
-- Entradas: ruta de un archivo existente
-- Salidas: imprime el resumen de la carga
-- Restricciones: las lineas invalidas y los nombres repetidos se omiten
importRoomTypes :: FilePath -> IO ()
importRoomTypes path = do
    fileTypes <- map normalizeRoomType <$> getRtFromTxt path
    totalLines <- countDataLines path
    existing <- getRoomTypes
    let validTypes = filter isValidRoomType fileTypes
        (newTypes, repeatedTypes) = splitNewRoomTypes existing validTypes
    mapM_ addRoomType newTypes
    printLoadSummary newTypes (length repeatedTypes) (totalLines - length validTypes)

-- Objetivo: contar las lineas con contenido de un archivo
-- Entradas: ruta del archivo
-- Salidas: cantidad de lineas que no estan en blanco
-- Restricciones: el archivo debe existir
countDataLines :: FilePath -> IO Int
countDataLines path = do
    contents <- readFile path
    pure (length (filter (not . isBlank) (lines contents)))

-- Objetivo: mostrar el resumen de una carga de tipos
-- Entradas: tipos agregados, cantidad de tipos repetidos omitidos y de lineas invalidas omitidas
-- Salidas: imprime la tabla de tipos agregados y los contadores
-- Restricciones: si no se agrego ningun tipo solo se muestran los contadores
printLoadSummary :: [RoomType] -> Int -> Int -> IO ()
printLoadSummary added repeated invalid = do
    putStrLn ""
    if null added
        then putStrLn "No se agregó ningún tipo nuevo."
        else do
            printSuccess ("Tipos agregados: " ++ show (length added))
            putStrLn ""
            printLines (formatTable ["Nombre", "Descripción", "Máx. huéspedes"] (map roomTypeRow added))
    putStrLn ""
    putStrLn ("Tipos que ya existían (omitidos): " ++ show repeated)
    putStrLn ("Líneas con formato inválido (omitidas): " ++ show invalid)

-- ---------------------------------------------------------------
-- Asignar cantidad de habitaciones por tipo
-- ---------------------------------------------------------------

-- Objetivo: opcion administrativa para generar las habitaciones de cada tipo
-- Entradas: pide las cantidades al usuario
-- Salidas: imprime el resultado
-- Restricciones: solo se puede realizar una vez
actionAssignRooms :: IO ()
actionAssignRooms = do
    printPanel "ASIGNAR HABITACIONES POR TIPO"
    existingRooms <- concat <$> getRooms
    roomTypes <- getRoomTypes
    case assignmentBlocker existingRooms roomTypes of
        Just problem -> printError problem
        Nothing      -> requestRoomPlan roomTypes

-- Objetivo: pedir la cantidad de habitaciones de cada tipo y confirmar el plan
-- Entradas: tipos de habitacion registrados
-- Salidas: genera las habitaciones si el usuario confirma
-- Restricciones: se debe asignar al menos una habitacion
requestRoomPlan :: [RoomType] -> IO ()
requestRoomPlan roomTypes = do
    putStrLn ("\nIndique la cantidad de habitaciones por tipo (0 a " ++ show maxRoomsPerType ++ "):")
    quantities <- mapM askRoomQuantity roomTypes
    confirmRoomPlan (buildRoomPlan roomTypes quantities)

-- Objetivo: preguntar la cantidad de habitaciones de un tipo
-- Entradas: el tipo de habitacion
-- Salidas: la cantidad indicada
-- Restricciones: entre 0 y el maximo permitido por tipo
askRoomQuantity :: RoomType -> IO Int
askRoomQuantity roomTypeValue =
    readIntInRange
        ("  " ++ rtName roomTypeValue ++ " (máx. huéspedes " ++ show (rtMaxGuests roomTypeValue) ++ "): ")
        0
        maxRoomsPerType

-- Objetivo: mostrar el plan de habitaciones y pedir confirmacion
-- Entradas: plan de (tipo, cantidad)
-- Salidas: genera las habitaciones si el usuario confirma
-- Restricciones: un plan vacio no se puede confirmar
confirmRoomPlan :: [(String, Int)] -> IO ()
confirmRoomPlan plan
    | null plan = printError "Debe asignar al menos una habitación."
    | otherwise = do
        putStrLn "\nResumen de la asignación:"
        printLines (formatTable ["Tipo", "Cantidad"] (map planRow plan))
        confirmed <- readYesNo "\n¿Confirmar la generación de habitaciones? (s/n): "
        if confirmed
            then generateRooms plan
            else putStrLn "Operación cancelada. No se generó ninguna habitación."

-- Objetivo: generar las habitaciones segun el plan
-- Entradas: plan de (tipo, cantidad)
-- Salidas: guarda las habitaciones
-- Restricciones: usa identificadores autogenerados 
generateRooms :: [(String, Int)] -> IO ()
generateRooms plan = do
    mapM_ (uncurry populateRoomsOfType) plan
    printSuccess ("Se generaron " ++ show (sum (map snd plan)) ++ " habitaciones en total.")

-- ---------------------------------------------------------------
-- Ver habitaciones
-- ---------------------------------------------------------------

-- Objetivo: opcion administrativa para ver todas las habitaciones
-- Entradas: ninguna
-- Salidas: imprime id y tipo de cada habitacion
-- Restricciones: requiere habitaciones generadas
actionViewRooms :: IO ()
actionViewRooms = do
    printPanel "HABITACIONES DEL HOTEL"
    withGeneratedRooms showRooms

-- Objetivo: mostrar una lista de habitaciones
-- Entradas: habitaciones del hotel
-- Salidas: imprime en consola
-- Restricciones: ninguna
showRooms :: [Room] -> IO ()
showRooms rooms = do
    putStrLn ""
    printLines (formatTable ["ID", "Tipo"] (map roomRow (sortOn roomId rooms)))
    putStrLn ("\nTotal de habitaciones: " ++ show (length rooms))

-- ---------------------------------------------------------------
-- Carga de tarifas
-- ---------------------------------------------------------------

-- Objetivo: opcion administrativa para cargar las tarifas desde un archivo
-- Entradas: pide la ruta al usuario
-- Salidas: imprime el resultado
-- Restricciones: cada carga reemplaza las tarifas anteriores
actionLoadRates :: IO ()
actionLoadRates = do
    printPanel "CARGA DE TARIFAS"
    maybePath <- readOptionalPath "Ruta del archivo de tarifas (enter para cancelar): "
    case maybePath of
        Nothing   -> putStrLn "Operación cancelada."
        Just path -> loadRatesFromFile path

-- Objetivo: cargar tarifas si el archivo existe
-- Entradas: ruta del archivo
-- Salidas: imprime el resultado
-- Restricciones: muestra un error si el archivo no existe
loadRatesFromFile :: FilePath -> IO ()
loadRatesFromFile path = do
    exists <- doesFileExist path
    if exists
        then importRates path
        else printError "El archivo indicado no existe."

-- Objetivo: leer, validar y guardar las tarifas de un archivo
-- Entradas: ruta de un archivo existente
-- Salidas: imprime las tarifas cargadas
-- Restricciones: el archivo debe tener las 8 tarifas con montos validos
importRates :: FilePath -> IO ()
importRates path = do
    rates <- getRatesFromTxt path
    if isValidRateSet rates
        then saveRates (sortOn rateId rates)
        else printError "El archivo debe tener 8 tarifas válidas."

-- Objetivo: guardar las tarifas y mostrarlas
-- Entradas: tarifas ordenadas por id
-- Salidas: guarda y imprime en consola
-- Restricciones: reemplaza las tarifas anteriores
saveRates :: [Rate] -> IO ()
saveRates rates = do
    loadRates rates
    printSuccess "Tarifas cargadas correctamente."
    putStrLn ""
    printLines (formatTable ["ID", "Descripción", "Monto por persona"] (map rateRow rates))

-- ---------------------------------------------------------------
-- Consultar reservaciones
-- ---------------------------------------------------------------

-- Objetivo: opcion administrativa para ver el historial de reservaciones
-- Entradas: ninguna
-- Salidas: imprime todas las reservaciones
-- Restricciones: ninguna
actionConsultReservations :: IO ()
actionConsultReservations = do
    printPanel "HISTORIAL DE RESERVACIONES"
    reservations <- getReservations
    if null reservations
        then putStrLn "\nNo hay reservaciones registradas."
        else do
            mapM_ showReservation (sortOn resId reservations)
            putStrLn ("\nTotal de reservaciones: " ++ show (length reservations))

-- Objetivo: mostrar el detalle de una reservacion precedido por una linea en blanco
-- Entradas: una reservacion
-- Salidas: imprime en consola
-- Restricciones: ninguna
showReservation :: Reservation -> IO ()
showReservation reservation = do
    putStrLn ""
    printLines (reservationEntryLines reservation)

-- ---------------------------------------------------------------
-- Consultar estado de ocupacion
-- ---------------------------------------------------------------

-- Objetivo: opcion administrativa para ver el estado de ocupacion de un dia
-- Entradas: pide la fecha al usuario
-- Salidas: imprime habitaciones disponibles y ocupadas
-- Restricciones: requiere habitaciones generadas
actionConsultOccupancy :: IO ()
actionConsultOccupancy = do
    printPanel "ESTADO DE OCUPACIÓN"
    withGeneratedRooms (\_ -> do
        date <- readValidDate "Día a consultar (dd/mm/aaaa): "
        freeRooms <- availableRooms date
        busyRooms <- occupiedRooms date
        putStrLn ""
        printLines (occupancyReportLines date freeRooms busyRooms))
