module RoomTypeDM (getRoomTypes, addRoomType, getRtFromTxt) where

import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)
import Types (RoomType (..))

splitOnChar :: Char -> String -> [String]
splitOnChar delimiter str = foldr go [""] str
  where
    go c (x:xs)
      | c == delimiter = "" : x : xs
      | otherwise      = (c : x) : xs


dataFile :: FilePath
dataFile = "program/data/typesDB.txt"

getRoomTypes :: IO [RoomType]
getRoomTypes = do
    contents <- readFile dataFile
    _ <- evaluate (length contents) -- evaluar la longitud para forzar que Haskell no sea perezoso.
    pure (read contents)

addRoomType :: RoomType -> IO ()
addRoomType newRoomType = do
    roomTypes <- getRoomTypes
    if any ((== rtName newRoomType) . rtName) roomTypes
        then print $ "El tipo de habitación " ++ rtName newRoomType ++ " ya existe. No se agregó."
        else do
            let updatedRoomTypes = newRoomType : roomTypes
            writeFile dataFile (show updatedRoomTypes)
            print $ "Tipo de habitación agregado: " ++ rtName newRoomType

roomTypeListToType :: [String] -> Maybe RoomType
roomTypeListToType [name, description, maxGuestsStr] =
    Just (RoomType name description (read maxGuestsStr))
roomTypeListToType _ = Nothing

getRtFromTxt :: String -> IO [RoomType]
getRtFromTxt path = do
    contents <- readFile path
    let lineList = lines contents
    let parsedRoomTypes = map (splitOnChar ',') lineList
    let roomTypes = mapMaybe roomTypeListToType parsedRoomTypes
    pure roomTypes




