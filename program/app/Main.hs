module Main where

import GHC.IO.Encoding (setLocaleEncoding)
import System.IO (hSetEncoding, stdin, stdout, utf8)

import UI.Display (printError, printPanel)
import UI.MainMenu (runMainMenu)
import Utils.DataInit (ensureDataFiles, isDataDirectoryAvailable)

-- Objetivo: iniciar el programa principal
-- Entradas: ninguna
-- Salidas: el flujo principal del programa o la finalizacion de este
-- Restricciones: debe ejecutarse desde la carpeta raiz, la que contiene la carpeta "program"
main :: IO ()
main = do
    configureEncoding
    available <- isDataDirectoryAvailable
    if available
        then startSystem
        else printError "No se encontró la carpeta program/data."

-- Objetivo: preparar los archivos de "data", mostrar el menu principal y finalizar el programa
-- Entradas: ninguna
-- Salidas: ninguna
-- Restricciones: la carpeta de datos debe existir
startSystem :: IO ()
startSystem = do
    ensureDataFiles
    runMainMenu
    printPanel "¡Gracias por usar el sistema!"

-- Objetivo: usar UTF-8 en la consola y en los archivos para evitar errores con tildes
-- Entradas: ninguna
-- Salidas: ninguna
-- Restricciones: ninguna
configureEncoding :: IO ()
configureEncoding = do
    setLocaleEncoding utf8
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
