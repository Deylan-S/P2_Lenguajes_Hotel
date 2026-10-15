module Utils.InputUtils where

import Text.Read (readMaybe)
import System.IO (hFlush, stdout)


-- | Solicita un número entero hasta que el usuario ingrese uno válido.
-- Ideal para IDs, cantidades de personas, etc.
safeReadInt :: String -> IO Int
safeReadInt prompt = do
    putStr prompt
    hFlush stdout -- Asegura que el prompt se imprima antes de esperar la entrada
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just number -> return number
        Nothing -> do
            putStrLn "[!] ERROR: Número entero inválido. Por favor, intente de nuevo."
            safeReadInt prompt

-- | Solicita un texto asegurándose de que no esté vacío.
-- Ideal para nombres, tipos de habitación, etc.
safeReadString :: String -> IO String
safeReadString prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    if null input
        then do
            putStrLn "[!] Error: La entrada no puede estar vacía. Por favor, intente de nuevo."
            safeReadString prompt
        else return input

-- | Solicita un número flotante/decimal de forma segura.
-- Útil para tarifas o montos si es necesario
safeReadDouble :: String -> IO Double
safeReadDouble prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    case readMaybe input :: Maybe Double of
        Just number -> return number
        Nothing -> do
            putStrLn "[!] Error: Número inválido. Por favor, intente de nuevo."
            safeReadDouble prompt

-- Pausa la ejecución hasta que el usuario presione Enter
pauseScreen :: IO ()
pauseScreen = do
    putStr "\nPresione ENTER para continuar "
    hFlush stdout
    _ <- getLine
    return ()

-- Limpia la terminal y coloca el cursor en la esquina superior izquierda
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"
