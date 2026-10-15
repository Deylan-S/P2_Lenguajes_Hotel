module UI.Common
    ( runSafely
    , runAction
    , withGeneratedRooms
    ) where

import Control.Exception (SomeException, catch)

import RoomDM (getRooms)
import Types (Room)
import UI.Display (printError)
import Utils.InputUtils (clearScreen, pauseScreen)

-- Objetivo: ejecutar una accion sin que un error inesperado cierre el programa
-- Entradas: una accion de entrada y salida
-- Salidas: ninguna (si hay un error lo muestra y continua)
-- Restricciones: ninguna
runSafely :: IO () -> IO ()
runSafely action = action `catch` handleFailure

-- Objetivo: mostrar un error inesperado al usuario
-- Entradas: la excepcion capturada
-- Salidas: imprime en consola
-- Restricciones: ninguna
handleFailure :: SomeException -> IO ()
handleFailure failure =
    printError ("Ocurrió un problema inesperado: " ++ show failure)

-- Objetivo: ejecutar una opcion de menu limpiando la pantalla y pausando al terminar
-- Entradas: la accion de la opcion
-- Salidas: ninguna
-- Restricciones: el menu se vuelve a dibujar despues de la pausa
runAction :: IO () -> IO ()
runAction action = do
    clearScreen
    runSafely action
    pauseScreen

-- Objetivo: ejecutar una accion solo si ya existen habitaciones generadas
-- Entradas: una accion que recibe todas las habitaciones del hotel
-- Salidas: ninguna (si no hay habitaciones muestra un aviso)
-- Restricciones: ninguna
withGeneratedRooms :: ([Room] -> IO ()) -> IO ()
withGeneratedRooms action = do
    rooms <- concat <$> getRooms
    if null rooms
        then printError "Aún no se han generado las habitaciones del hotel."
        else action rooms
