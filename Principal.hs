-- En este archivo se realiza el control interactivo del programa con el usuario

import System.IO
import AgregacionSR
import AgregacionCR

funcionZDT3 = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nFunción ZDT3 seleccionada"
    putStr "\nIndique tamaño de población: "
    p <- getChar
    putStr "\nIndique número de generaciones: "
    g <- getChar
    putStr "\nIndique tamaño de vecindad de un individuo (entre 0.1 - 0.3) : "
    t <- getChar
    putStr "\nIndique porcentaje de mutación (entre 0.1 - 0.9) : "
    f <- getChar
    putStr "\nIndique porcentaje de cruce (entre 0.1 - 0.9) : "
    cr <- getChar
    putStr "\nEjecutando algoritmo..."
     <- algoritmo_agregacion_ZDT3
    return ()

funcionCF6_4 = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nFunción CF6 4 seleccionada"
    putStr "\nIndique tamaño de población: "
    p <- getChar
    putStr "\nIndique número de generaciones: "
    g <- getChar
    putStr "\nIndique tamaño de vecindad de un individuo (entre 0.1 - 0.3) : "
    t <- getChar
    putStr "\nIndique porcentaje de mutación (entre 0.1 - 0.9) : "
    f <- getChar
    putStr "\nIndique porcentaje de cruce (entre 0.1 - 0.9) : "
    cr <- getChar
    return ()

funcionCF6_16 = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nFunción CF6 16 seleccionada"
    putStr "\nIndique tamaño de población: "
    p <- getChar
    putStr "\nIndique número de generaciones: "
    g <- getChar
    putStr "\nIndique tamaño de vecindad de un individuo (entre 0.1 - 0.3) : "
    t <- getChar
    putStr "\nIndique porcentaje de mutación (entre 0.1 - 0.9) : "
    f <- getChar
    putStr "\nIndique porcentaje de cruce (entre 0.1 - 0.9) : "
    cr <- getChar
    return ()

limpiar = putStr "\ESC[2J"

main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Seleccione la función que desea optimizar:\n"
    putStrLn "\t1. Función ZDT3 (30 dimensiones)"
    putStrLn "\t2. Función CF6  (4  dimensiones)"
    putStrLn "\t3. Función CF6  (16 dimensiones)"
    putStrLn "\t4. Salir"
    putStr "\nEscriba el número asociado a la función: "
    o <- getChar
    case o of '1' -> funcionZDT3
              '2' -> funcionCF6_4
              '3' -> funcionCF6_16
              '4' -> return ()
              _ -> putStrLn "\nNo ha seleccionado un indice correcto, vuelva a intentarlo" >> main