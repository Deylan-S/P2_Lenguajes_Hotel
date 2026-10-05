module Utils where

-- Función para dividir una cadena en una lista de subcadenas utilizando un carácter delimitador.
splitOnChar :: Char -- ^ El carácter delimitador
            -> String -- ^ La cadena de entrada
            -> [String] -- ^ La lista de subcadenas resultante
splitOnChar delimiter str = foldr go [""] str
  where
    go c (x:xs)
      | c == delimiter = "" : x : xs
      | otherwise      = (c : x) : xs