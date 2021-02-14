module Funciones.FileTreatment
(guardar_datos_zdt3,
guardar_datos_cf6,
cargaDatos
) where
    -- Este fichero contendrá todas las funciones relacionadas 
-- con la lectura y escritura sobre ficheros
-- =======================================================

import Data.Char
import Data.Time
import SolucionCF6
import SolucionZDT3

cargaDatos :: [Char] -> IO [(Double, Double)]
cargaDatos fileName = do  
        let file = fileName
        dat <- readFile fileName
        let lineas = lines dat
        return (aNumero (datos lineas)) 


split :: Eq a => a -> [a] -> [[a]]
split x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if y==x then 
            func x ys ([]:(z:zs)) 
        else 
            func x ys ((y:z):zs)

datos :: Foldable t => t [Char] -> [[[Char]]]
datos lineas = foldr (\palabra js-> split '\t' palabra:js) [] lineas

aNumero :: [[String]] -> [(Double, Double)]
aNumero [] = []
aNumero (x:xss) = (read (x!!0),read (x!!1)):aNumero xss


guardar_datos_cf6 last_gen = do
    time<-getCurrentTime
    let escribe = [(show x ++ "\t" ++ show y ++ "\n")|(x,y)<-last_gen]
    writeFile ("out/Datos_CF6"++ (take 10 (show time)) ++ ".dat") (unwords escribe)
    return ()

guardar_datos_zdt3 last_gen = do
    time<-getCurrentTime
    let escribe = [(show x ++ "\t" ++ show y ++ "\n")|(x,y)<-last_gen]
    writeFile ("out/Datos_ZDT3"++ (take 10 (show time)) ++".dat") (unwords escribe)
    return ()
   
-- F por Windows
-- plotDots [Key Nothing, XRange (-0.1,1.1), YRange(-1.1,1.1)] a
