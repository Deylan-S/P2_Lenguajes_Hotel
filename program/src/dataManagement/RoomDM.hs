module RoomDM where

import Types (Room (..))
import Control.Exception (evaluate)

dataFile :: FilePath
dataFile = "program/data/roomsDB.txt"

getRooms :: IO [[Room]]
getRooms = do
    contents <- readFile dataFile
    _ <- evaluate (length contents)
    pure (read contents)

getLastRoomId :: IO Int
getLastRoomId = do
    rooms <- getRooms
    let allRooms = concat rooms
    pure $ if null allRooms then 0 else maximum (map roomId allRooms)

populateRoomsOfType :: String -- ^ El tipo de habitación a popular
                    -> Int    -- ^ La cantidad de habitaciones a crear
                    -> IO ()
populateRoomsOfType newRoomType count = do
    rooms <- getRooms
    if any (any ((== newRoomType) . roomType)) rooms
        then putStrLn $ "Ya existen habitaciones del tipo " ++ newRoomType ++ ". No se agregaron nuevas habitaciones."
    else do
        lastId <- getLastRoomId
        if lastId == 0
            then do
                let newRooms = [Room (i + 1) newRoomType | i <- [0..(count - 1)]]
                writeFile dataFile (show [newRooms])
            else do
                let newRooms = [Room (i + lastId + 1) newRoomType | i <- [0..(count - 1)]]
                let updatedRooms = rooms ++ [newRooms]
                writeFile dataFile (show updatedRooms)
        putStrLn $ "Se han agregado " ++ show count ++ " habitaciones del tipo " ++ newRoomType ++ "."