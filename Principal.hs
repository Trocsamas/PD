-- En este archivo se realiza el control interactivo del programa con el usuario

import System.IO

main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Seleccione la función que desea optimizar:"
    putStrLn "\t 1. Función ZDT3 (30 dimensiones)"
    putStrLn "\t 2. Función CF6  (4  dimensiones)"
    putStrLn "\t 3. Función CF6  (16 dimensiones)"