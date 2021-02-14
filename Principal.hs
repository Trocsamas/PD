-- En este archivo se realiza el control interactivo del programa con el usuario

import System.IO
import AgregacionSR
import AgregacionCR
import SolucionZDT3
import SolucionCF6

funcionZDT3 = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nFunción ZDT3 seleccionada"
    putStr "\nIndique tamaño de población: "
    poblacion <- getLine
    putStr "\nIndique número de generaciones: "
    generaciones <- getLine
    putStr "\nIndique tamaño de vecindad de un individuo (entre 0.1 - 0.3) (se recomienda 0.2) : "
    tamano <- getLine
    putStr "\nIndique porcentaje de mutación (entre 0.1 - 0.9) (se recomienda 0.5) : "
    mutacion <- getLine
    putStr "\nIndique porcentaje de cruce (entre 0.1 - 0.9) (se recomienda 0.5) : "
    cruce <- getLine
    putStrLn "\nEjecutando algoritmo..."
    let p = read poblacion :: Int
    let g = read generaciones :: Int
    let t = read tamano :: Double
    let f = read mutacion :: Double
    let cr = read cruce :: Double
    solucion <- algoritmo_agregacion_ZDT3 p g t f cr 0 1
    let final = nuevaSolucionZDT3 solucion
    putStr $ show (selecciona_evaluaciones_de_generacionZDT3 100 final)
    return ()

funcionCF6_4 = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nFunción CF6 4 seleccionada"
    putStr "\nIndique tamaño de población: "
    poblacion <- getLine
    putStr "\nIndique número de generaciones: "
    generaciones <- getLine
    putStr "\nIndique tamaño de vecindad de un individuo (entre 0.1 - 0.3) (se recomienda 0.2) : "
    tamano <- getLine
    putStr "\nIndique porcentaje de mutación (entre 0.1 - 0.9) (se recomienda 0.5) : "
    mutacion <- getLine
    putStr "\nIndique porcentaje de cruce (entre 0.1 - 0.9) (se recomienda 0.5) : "
    cruce <- getLine
    putStrLn "\nEjecutando algoritmo..."
    let p = read poblacion :: Int
    let g = read generaciones :: Int
    let t = read tamano :: Double
    let f = read mutacion :: Double
    let cr = read cruce :: Double
    solucion <- algoritmo_agregacion_restricciones p g t f cr 4
    let final = nuevaSolucionCF6 solucion

    

    putStr $ show (selecciona_evaluaciones_de_generacionCF6 100 final)
    return ()

funcionCF6_16 = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nFunción CF6 16 seleccionada"
    putStr "\nIndique tamaño de población: "
    poblacion <- getLine
    putStr "\nIndique número de generaciones: "
    generaciones <- getLine
    putStr "\nIndique tamaño de vecindad de un individuo (entre 0.1 - 0.3) (se recomienda 0.2) : "
    tamano <- getLine
    putStr "\nIndique porcentaje de mutación (entre 0.1 - 0.9) (se recomienda 0.5) : "
    mutacion <- getLine
    putStr "\nIndique porcentaje de cruce (entre 0.1 - 0.9) (se recomienda 0.5) : "
    cruce <- getLine
    putStrLn "\nEjecutando algoritmo..."
    let p = read poblacion :: Int
    let g = read generaciones :: Int
    let t = read tamano :: Double
    let f = read mutacion :: Double
    let cr = read cruce :: Double
    solucion <- algoritmo_agregacion_restricciones p g t f cr 16
    let final = nuevaSolucionCF6 solucion
    putStr $ show (selecciona_evaluaciones_de_generacionCF6 100 final)
    return ()

main = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nSeleccione la función que desea optimizar:\n"
    putStrLn "\t1. Función ZDT3 (30 dimensiones)"
    putStrLn "\t2. Función CF6  (4  dimensiones)"
    putStrLn "\t3. Función CF6  (16 dimensiones)"
    putStrLn "\t4. Salir"
    putStr "\nEscriba el número asociado a la selección: "
    o <- getLine
    case o of "1" -> funcionZDT3
              "2" -> funcionCF6_4
              "3" -> funcionCF6_16
              "4" -> return ()
              _ -> putStrLn "\nNo ha seleccionado un indice correcto, vuelva a intentarlo" >> main