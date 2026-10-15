module MenuManagement.MenuEngine where

import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import Utils.InputUtils (pauseScreen, clearScreen)

-- Objetivo: definir el tipo para las opciones del menu interactivo
-- Entradas: ninguna
-- Salidas: ninguna
-- Restricciones: empareja una etiqueta de texto con una accion IO
type MenuOption = (String, IO ())

-- Objetivo: calcular las dimensiones y los textos formateados del menu
-- Entradas: el titulo del menu y la lista de opciones
-- Salidas: tupla con (lineas de opciones, linea de salida, ancho del contenido, texto del borde)
-- Restricciones: el ancho minimo es de 48
buildMenuLayout :: String -> [MenuOption] -> ([String], String, Int, String)
buildMenuLayout title options =
    let 
        optionLines = zipWith formatOption [1 :: Int ..] options
        exitLine = "  [0]  Salir / Volver"
        widestLine = maximum (map length (title : optionLines ++ [exitLine]))
        contentWidth = max 48 (widestLine + 4)
        border = "+" ++ replicate (contentWidth + 2) '-' ++ "+"
    in 
        (optionLines, exitLine, contentWidth, border)

-- Objetivo: renderizar la interfaz visual del menu en la consola
-- Entradas: titulo, lineas de opciones, linea de salida, ancho del contenido y borde
-- Salidas: ninguna
-- Restricciones: ninguna
drawMenu :: String -> [String] -> String -> Int -> String -> IO ()
drawMenu title optionLines exitLine contentWidth border = do
    -- Dibujar encabezado
    putStrLn ""
    putStrLn border
    printMenuRow contentWidth (centerText contentWidth title)
    putStrLn border

    -- Dibujar opciones disponibles
    printMenuRow contentWidth ""
    mapM_ (printMenuRow contentWidth) optionLines

    -- Dibujar opción de salida
    printMenuRow contentWidth ""
    printMenuRow contentWidth exitLine
    putStrLn border

-- Objetivo: controlar el ciclo principal y la logica de interaccion de un menu
-- Entradas: el titulo del menu y las opciones a mostrar
-- Salidas: el flujo del menu seleccionado
-- Restricciones: ninguna
runMenu :: String -> [MenuOption] -> IO ()
runMenu title options = do
    clearScreen

    let (optionLines, exitLine, contentWidth, border) = buildMenuLayout title options

    drawMenu title optionLines exitLine contentWidth border

    putStrLn ""
    putStr "Seleccione una opción: "
    hFlush stdout
    input <- getLine

    case readMaybe input :: Maybe Int of
        Just 0 -> do
            return ()

        Just n | n > 0 && n <= length options -> do
            let (_, action) = options !! (n - 1)
            action
            runMenu title options

        _ -> do
            putStrLn "\n[ERROR] Opción inválida. Intente de nuevo."
            pauseScreen
            runMenu title options

-- Objetivo: dar formato a una opcion del menu agregandole su numero de seleccion
-- Entradas: el numero de posicion y la opcion del menu
-- Salidas: el texto de la opcion ya formateado
-- Restricciones: ninguna
formatOption :: Int -> MenuOption -> String
formatOption number (label, _) =
    "  [" ++ show number ++ "]  " ++ label

-- Objetivo: imprimir una linea de texto ajustada a los bordes laterales del menu
-- Entradas: el ancho del contenido y el texto a imprimir
-- Salidas: ninguna
-- Restricciones: ninguna
printMenuRow :: Int -> String -> IO ()
printMenuRow width text =
    putStrLn
        ("| " ++ text ++ replicate (width - length text) ' ' ++ " |")

-- Objetivo: centrar un texto dado un ancho especifico
-- Entradas: el ancho disponible y el texto
-- Salidas: el texto rellenado con espacios a ambos lados
-- Restricciones: ninguna
centerText :: Int -> String -> String
centerText width text =
    let leftPadding = (width - length text) `div` 2
        rightPadding = width - length text - leftPadding
    in replicate leftPadding ' '
       ++ text
       ++ replicate rightPadding ' '