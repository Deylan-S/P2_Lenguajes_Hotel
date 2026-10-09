module RatesDM where

import Types (Rate (..))
import Control.Exception (evaluate)
import System.Directory (doesFileExist)

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
            let lineList = lines contents
            let lineCount = length lineList
            let rates = [Rate i (read (lineList !! (i - 1))) | i <- [1..(lineCount)]]
            pure rates

loadRates :: [Rate] -> IO ()
loadRates newRates = do
    writeFile dataFile (show newRates)
