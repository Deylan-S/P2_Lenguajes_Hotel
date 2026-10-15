module UI.Display
    ( printPanel
    , printLines
    , printError
    , printSuccess
    ) where

-- Objetivo: imprimir un mensaje dentro de un panel con bordes
-- Entradas: el mensaje a mostrar
-- Salidas: imprime en consola
-- Restricciones: el mensaje debe ocupar una sola linea
printPanel :: String -> IO ()
printPanel message = do
    let contentWidth = max 48 (length message + 4)
        border = "+" ++ replicate (contentWidth + 2) '-' ++ "+"
        leftPadding = (contentWidth - length message) `div` 2
        rightPadding = contentWidth - length message - leftPadding
    putStrLn ""
    putStrLn border
    putStrLn ("| " ++ replicate leftPadding ' ' ++ message ++ replicate rightPadding ' ' ++ " |")
    putStrLn border

-- Objetivo: imprimir una lista de lineas de texto
-- Entradas: lista de lineas
-- Salidas: imprime en consola
-- Restricciones: ninguna
printLines :: [String] -> IO ()
printLines = mapM_ putStrLn

-- Objetivo: imprimir un mensaje de error
-- Entradas: el mensaje
-- Salidas: imprime en consola
-- Restricciones: ninguna
printError :: String -> IO ()
printError message = putStrLn ("[!] Error: " ++ message)

-- Objetivo: imprimir un mensaje de exito
-- Entradas: el mensaje
-- Salidas: imprime en consola
-- Restricciones: ninguna
printSuccess :: String -> IO ()
printSuccess message = putStrLn ("[OK] " ++ message)
