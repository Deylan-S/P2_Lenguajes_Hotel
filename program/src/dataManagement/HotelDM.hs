module HotelDM (getHotelInfo) where

import Types (HotelInfo)

dataFile :: FilePath
dataFile = "program/data/hotel.txt"

getHotelInfo :: IO HotelInfo
getHotelInfo = read <$> readFile dataFile
