module ReservationDM (getReservations, addReservation) where

import Control.Exception (evaluate)
import Types (Reservation (..), RoomOccupancy (..), ReservationStatus (..), DateTime (..), Date (..))

filePath :: FilePath
filePath = "program/data/reservationsDB.txt"

getReservations :: IO [Reservation]
getReservations = do
    contents <- readFile filePath
    _ <- evaluate (length contents) -- evaluar la longitud para forzar que Haskell no sea perezoso.
    pure (read contents)

addReservation :: Reservation -> IO ()
addReservation newReservation = do
    reservations <- getReservations
    let updatedReservations = newReservation : reservations
    writeFile filePath (show updatedReservations)