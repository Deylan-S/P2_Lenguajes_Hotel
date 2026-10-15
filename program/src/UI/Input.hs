module UI.Input
    ( readRawLine
    , readNonEmptyText
    , readPersonName
    , readIntInRange
    , readPositiveInt
    , readValidDate
    , readYesNo
    , readOptionalPath
    ) where

import Data.Char (isAlpha, toLower)
import System.IO (hFlush, stdout)

import Types (Date)
import Utils.DateUtils (parseDate)
import Utils.InputUtils (safeReadInt, safeReadString)
import Utils.TextUtils (stripQuotes, trim)

-- Objetivo: leer una linea sin ninguna validacion
-- Entradas: el mensaje a mostrar
-- Salidas: la linea escrita por el usuario
-- Restricciones: ninguna
readRawLine :: String -> IO String
readRawLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

-- Objetivo: leer un texto que no este vacio ni solo con espacios
-- Entradas: el mensaje a mostrar
-- Salidas: el texto sin espacios en los extremos
-- Restricciones: repite la pregunta hasta recibir un texto valido
readNonEmptyText :: String -> IO String
readNonEmptyText prompt = do
    input <- safeReadString prompt
    let cleaned = trim input
    if null cleaned
        then do
            putStrLn "[!] Error: La entrada no puede estar vacía."
            readNonEmptyText prompt
        else pure cleaned

-- Objetivo: leer el nombre de una persona
-- Entradas: el mensaje a mostrar
-- Salidas: el nombre sin espacios en los extremos
-- Restricciones: solo letras, espacios, punto, guion y apostrofe; maximo 60 caracteres
readPersonName :: String -> IO String
readPersonName prompt = do
    name <- readNonEmptyText prompt
    if length name <= 60 && all isNameChar name
        then pure name
        else do
            putStrLn "[!] Error: El nombre solo puede tener letras y espacios."
            readPersonName prompt
  where
    isNameChar c = isAlpha c || c `elem` " .-'"

-- Objetivo: leer un entero dentro de un rango
-- Entradas: mensaje, limite inferior y limite maximo
-- Salidas: el entero leido
-- Restricciones: repite la pregunta hasta recibir un valor dentro del rango
readIntInRange :: String -> Int -> Int -> IO Int
readIntInRange prompt low high = do
    value <- safeReadInt prompt
    if value >= low && value <= high
        then pure value
        else do
            putStrLn ("[!] Error: El valor debe estar entre " ++ show low ++ " y " ++ show high ++ ".")
            readIntInRange prompt low high

-- Objetivo: leer un entero positivo
-- Entradas: el mensaje a mostrar
-- Salidas: un entero mayor o igual a 1
-- Restricciones: repite la pregunta hasta recibir un valor valido
readPositiveInt :: String -> IO Int
readPositiveInt prompt = readIntInRange prompt 1 maxBound

-- Objetivo: leer una fecha
-- Entradas: el mensaje a mostrar
-- Salidas: una fecha valida
-- Restricciones: formato dd/mm/aaaa; repite la pregunta hasta recibir una fecha valida
readValidDate :: String -> IO Date
readValidDate prompt = do
    input <- safeReadString prompt
    case parseDate input of
        Just date -> pure date
        Nothing -> do
            putStrLn "[!] Error: Fecha inválida. Use el formato dd/mm/aaaa."
            readValidDate prompt

-- Objetivo: leer una confirmacion de si o no
-- Entradas: el mensaje a mostrar
-- Salidas: True si la respuesta es afirmativa
-- Restricciones: acepta s, si, n, no (sin importar mayusculas); repite si es otra cosa
readYesNo :: String -> IO Bool
readYesNo prompt = do
    answer <- map toLower . trim <$> readRawLine prompt
    if answer `elem` ["s", "si"]
        then pure True
        else if answer `elem` ["n", "no"]
            then pure False
            else do
                putStrLn "[!] Error: Responda con 's' o 'n'."
                readYesNo prompt

-- Objetivo: leer una ruta de archivo opcional
-- Entradas: el mensaje a mostrar
-- Salidas: "Just ruta" sin comillas, o "Nothing" si el usuario deja la entrada vacia
-- Restricciones: no verifica que el archivo exista
readOptionalPath :: String -> IO (Maybe FilePath)
readOptionalPath prompt = do
    input <- readRawLine prompt
    let path = stripQuotes (trim input)
    pure (if null path then Nothing else Just path)
