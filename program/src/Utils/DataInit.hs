module Utils.DataInit
    ( isDataDirectoryAvailable
    , ensureDataFiles
    ) where

import Control.Exception (evaluate)
import Control.Monad (when)
import System.Directory (doesDirectoryExist, doesFileExist)

import Utils.TextUtils (isBlank)

-- Objetivo: carpeta donde se guardan los archivos de datos
-- Entradas: ninguna
-- Salidas: ruta relativa a la carpeta que contiene "program"
-- Restricciones: debe coincidir con las rutas de los modulos dataManagement
dataDirectory :: FilePath
dataDirectory = "program/data"

-- Objetivo: lista de bases de datos que deben contener una lista de Haskell
-- Entradas: ninguna
-- Salidas: rutas de los archivos
-- Restricciones: ninguna
databaseFiles :: [FilePath]
databaseFiles =
    map ((dataDirectory ++ "/") ++)
        ["roomsDB.txt", "typesDB.txt", "ratesDB.txt", "reservationsDB.txt", "invoicesDB.txt"]

-- Objetivo: saber si la carpeta de datos es accesible desde donde se ejecuta el programa
-- Entradas: ninguna
-- Salidas: True si existe la carpeta program/data
-- Restricciones: el programa debe ejecutarse desde la carpeta que contiene "program"
isDataDirectoryAvailable :: IO Bool
isDataDirectoryAvailable = doesDirectoryExist dataDirectory

-- Objetivo: dejar cada base de datos con una lista vacia si no existe o esta en blanco
-- Entradas: ninguna
-- Salidas: ninguna (puede crear o reescribir archivos)
-- Restricciones: no modifica archivos que ya tienen contenido
ensureDataFiles :: IO ()
ensureDataFiles = mapM_ ensureDatabase databaseFiles

-- Objetivo: inicializar una base de datos individual
-- Entradas: ruta del archivo
-- Salidas: ninguna
-- Restricciones: no modifica archivos que ya tienen contenido
ensureDatabase :: FilePath -> IO ()
ensureDatabase path = do
    exists <- doesFileExist path
    if not exists
        then writeFile path "[]"
        else do
            contents <- readFile path
            _ <- evaluate (length contents)
            when (isBlank contents) (writeFile path "[]")
