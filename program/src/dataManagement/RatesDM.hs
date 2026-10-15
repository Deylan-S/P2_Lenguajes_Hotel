module RatesDM where

import Types (Rate (..))
import Control.Exception (evaluate)
import System.Directory (doesFileExist)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Utils (splitOnChar)

dataFile :: FilePath
dataFile = "program/data/ratesDB.txt"

getRates :: IO [Rate]
getRates = do
    contents <- readFile dataFile
    _ <- evaluate (length contents)
    pure (read contents)

getRatesFromTxt :: FilePath -> IO [Rate]
getRatesFromTxt path = do
    exists <- doesFileExist path
    if not exists
        then do
            pure []
        else do
            contents <- readFile path
            _ <- evaluate (length contents)
            let dataLines = filter (not . all isSpace) (lines contents)
            pure (mapMaybe parseRateLine (zip [1 ..] dataLines))

-- Objetivo: convertir una linea del archivo en una tarifa
-- Entradas: posicion de la linea (1 en adelante) y el texto de la linea
-- Salidas: Just tarifa si el formato es valido, Nothing en caso contrario
-- Restricciones: formatos aceptados "monto" o "id,monto"
parseRateLine :: (Int, String) -> Maybe Rate
parseRateLine (position, line) =
    case splitOnChar ',' (filter (/= '\r') line) of
        [amountText]         -> Rate position <$> readMaybe amountText
        [idText, amountText] -> Rate <$> readMaybe idText <*> readMaybe amountText
        _                    -> Nothing

loadRates :: [Rate] -> IO ()
loadRates newRates = do
    writeFile dataFile (show newRates)
