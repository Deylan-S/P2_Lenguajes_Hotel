module Utils.TextUtils
    ( trim
    , isBlank
    , padRight
    , padLeft
    , pad2
    , stripQuotes
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- Objetivo: eliminar los espacios al inicio y al final de un texto
-- Entradas: un texto
-- Salidas: el texto sin espacios en los extremos
-- Restricciones: ninguna
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- Objetivo: saber si un texto esta vacio o solo tiene espacios
-- Entradas: un texto
-- Salidas: True si no tiene caracteres visibles
-- Restricciones: ninguna
isBlank :: String -> Bool
isBlank = all isSpace

-- Objetivo: rellenar un texto con espacios a la derecha
-- Entradas: ancho deseado y el texto
-- Salidas: el texto con al menos el ancho indicado
-- Restricciones: si el texto es mas largo que el ancho, no se recorta
padRight :: Int -> String -> String
padRight width text = text ++ replicate (width - length text) ' '

-- Objetivo: rellenar un texto con espacios a la izquierda
-- Entradas: ancho deseado y el texto
-- Salidas: el texto con al menos el ancho indicado
-- Restricciones: si el texto es mas largo que el ancho, no se recorta
padLeft :: Int -> String -> String
padLeft width text = replicate (width - length text) ' ' ++ text

-- Objetivo: mostrar un entero con al menos dos digitos
-- Entradas: un entero no negativo
-- Salidas: el entero como texto, con cero a la izquierda si es menor a 10
-- Restricciones: el numero debe ser mayor o igual a cero
pad2 :: Int -> String
pad2 n
    | n < 10    = '0' : show n
    | otherwise = show n

-- Objetivo: quitar las comillas que rodean un texto (rutas copiadas desde el explorador)
-- Entradas: un texto
-- Salidas: el texto sin comillas simples o dobles en los extremos
-- Restricciones: solo se quitan si abren y cierran con el mismo caracter
stripQuotes :: String -> String
stripQuotes text =
    case text of
        (open : rest@(_ : _))
            | isQuote open && last rest == open -> init rest
        _ -> text
  where
    isQuote c = c == '"' || c == '\''
