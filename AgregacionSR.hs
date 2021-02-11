-- Algoritmo multiobjetivo basado en agregación sin restriccion

import Funciones.Zdt3
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import Data.List
import Data.Array

import System.Random

ej1 = zdt3 [8.528997e-01,4.455033e-04,2.023997e-03,5.397792e-03,5.944923e-04,1.067563e-03,3.566097e-03,1.382548e-03,6.985887e-04,1.958344e-04,
          1.076955e-03,1.207479e-03,9.879777e-03,1.368514e-04,9.025464e-04,5.268854e-04,4.552294e-03,1.122561e-05,1.755626e-03,3.247557e-04,
          7.025530e-03,1.466073e-03,2.234936e-05,1.498200e-03,1.303323e-04,1.820097e-03,8.807117e-06,1.728758e-03,5.288978e-04,1.224419e-03]

-- Solución: 8.528997e-01   -7.643075e-01

dib2 = 
    plotList [] (take 30 fibs)
    where fibs :: [Double] 
          fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Prueba de muestreo de imagenes mediante GNUplot

someFunc :: IO ()
someFunc = putStrLn "someFunc"

foo = do
    let zs = [ (x,x) | x <- [1..10] ]
    plotList [Key Nothing
             ,YRange (0,maximum (map snd zs) + 1)
             ,XLabel "Days since launch of Hackage"
             ,YLabel "Unique uploads each day"
             ,Title "Daily uploads (180 day moving average) to http://hackage.haskell.org"
             ,Custom "grid" []
             -- ,SVG "/tmp/hackage-daily-graph.svg"
             -- ,Custom "terminal svg;set output \"/tmp/hackage-daily-graph.svg\""[]
             , terminal (SVG.cons "./out/output.svg")
             ,Custom "style line" ["3","lc","3","lw","3"]
             ] (map snd zs)
             

-- Funciones de Apoyo

roundTo :: Double -> Int -> Double
roundTo x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

parte :: [a] -> Int -> [[a]]
parte [] _ = []
parte xs n = (take n xs) : parte (drop n xs) n

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs

randomIndex :: [a] -> IO Int
randomIndex [] = error "Cannot select an element from an empty list."
randomIndex list = getStdRandom $ randomR (0, length list - 1)


-- Calculo de los vectores
calc_pesos :: (Enum b, Fractional b) => b -> [(b, b)]
calc_pesos n = [(0+paso*x,1-paso*x) | x <-[0..n-1]]
    where paso = 1/(n-1)

-- Calculo del punto Z

calc_z xs = [f1,f2]
    where f1 = minimum [ x ! 1 | x <- xs]
          f2 = minimum [ x ! 2 | x <- xs]

-- Calculo del Vencindario

calc_vecindario :: (Floating a1, Ord a1) => [(a1, a1)] -> Int -> Double -> [[Int]]
calc_vecindario xs n t = foldr (\xst ys -> f (take trunc (sort xst)):ys ) [] distss
    where distss = distancias xs
          trunc  = truncate (fromIntegral n*t)
          f yst  = foldr (\t js -> snd t :js) [] yst

distancia_euclidea :: Floating a => (a, a) -> (a, a) -> a
distancia_euclidea (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

distancias :: Floating a => [(a, a)] -> [[(a, Int)]]
distancias xs = parte [(distancia_euclidea i (xs !! j), j) | i <-xs, j <- [0..n-1]] n
    where n = (length xs)


-- Calculo de la Población Inicial

generaPoblacion :: Int -> IO [[Double]]
generaPoblacion n = do
    individuo <- generaIndividuo (30*n)
    return (parte individuo 30)

generaIndividuo :: Int -> IO [Double]
generaIndividuo n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
  return (take n xs)


-- Calculo de las Evaluaciones

evaluaciones :: Floating a => [[a]] -> [Funciones.Zdt3.Vector a]
evaluaciones [] = []
evaluaciones (x:xss) = zdt3 x : evaluaciones xss

-- Calculo de subproblemas
calc_subproblemas :: (Ix i, Num i, Num b, Ord b) => [Array i b] -> [(b, b)] -> [b] -> [b]
calc_subproblemas eval pesos z = maximo
    where resta = [((abs ((x!1)-z!!0),(abs ((x!2)-z!!1))))| x<-eval]
          maximo = calc_maximo (calc_subproblemas_aux resta pesos)

calc_subproblemas_aux :: (Num a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
calc_subproblemas_aux resta pesos = [((r1*p1),(r2*p2))| (r1,r2,p1,p2)<-xs]
    where (resta1,resta2) = unzip resta
          (pesos1,pesos2) = unzip pesos
          xs = zip4 resta1 resta2 pesos1 pesos2

calc_maximo :: (Foldable t, Ord a) => [t a] -> [a]
calc_maximo [] = []
calc_maximo (x:xs) = maximum x : calc_maximo xs
    
-- Calculo del vector Mutante

calc_mutante vecindario poblacion f cr min max = do
    seleccion <- seleccion_aleatoria poblacion vecindario
    let mutantes = mutaciones seleccion f min max
    mutantes_cruzados <- evolucion_diferencial mutantes poblacion cr
    mutantes_gaussianos <- mutaciones_gaussianas mutantes_cruzados
    let mutantes_finales = limitador mutantes_gaussianos min max
    return mutantes_finales

-- Funciones para el calculo del Vector Mutante

elige3Vecinos :: [a] -> IO [a]
elige3Vecinos xs = do
    let elegidos0 = []
    let lista0 = xs

    idx1 <- randomIndex lista0
    let elegidos1 = (lista0 !! idx1):elegidos0
    let lista1 = deleteAt idx1 lista0

    idx2 <- randomIndex lista1
    let elegidos2 = (lista1 !! idx2):elegidos1
    let lista2 = deleteAt idx2 lista1

    idx3 <- randomIndex lista2
    let elegidos3 = (lista2 !! idx3):elegidos2

    return elegidos3

seleccion_aleatoria :: [a] -> [[Int]] -> IO [[a]]
seleccion_aleatoria _ [] = do 
    return []
seleccion_aleatoria poblacion (xs:xss) = do
    elegidos <- elige3Vecinos xs
    seleccionados <- seleccion_aleatoria_aux poblacion elegidos
    resto <- seleccion_aleatoria poblacion xss
    let res = seleccionados:resto 
    return res

seleccion_aleatoria_aux :: [a] -> [Int] -> IO [a]
seleccion_aleatoria_aux _ [] = do
    return []
seleccion_aleatoria_aux poblacion (x:xs) = do
    let i = x
    resto <- seleccion_aleatoria_aux poblacion xs
    let res = (poblacion!!i):resto
    return res

-- mutaciones dentro del intervalo

mutaciones elegidos f min max = limitador [mutaciones_aux x f|x<-elegidos]  min max

limitador [] _ _ = []
limitador (xs:xss) min max = [(limitador_aux x min max)| x<-xs]:limitador xss min max

limitador_aux x min max
    | x<min = min
    | x>max = max
    |otherwise = x

mutaciones_aux (i0:i1:i2:_) f = zipWith (+) i0 [f*x| x<-(zipWith (-) i1 i2)]

-- calculo de cruces con vector mutante
-- cr porcentaje de cruce (Normalmente 0.5)

evolucion_diferencial :: [[a]] -> [[a]] -> Double -> IO [[a]]
evolucion_diferencial [] [] _ = do
    return []

--evolucion_diferencial :: [[a]] -> [[a]] -> Double -> IO [[a]]
evolucion_diferencial mutantes@(x:xs) individuos@(y:ys) cr = do
    nuevo_individuo <- cruce_individuo x y cr
    resto <- evolucion_diferencial xs ys cr
    let cruzados = nuevo_individuo:resto
    return cruzados

cruce_individuo mutante individuo cr = do
    cruces <- puntos_de_cruce cr
    let cruzados = [if cruces!!x then mutante!!x else individuo!!x | x <- [0..29]]
    return cruzados

puntos_de_cruce cr = do
    individuo <- generaIndividuo 30
    let cruces = [x < cr | x <- individuo]
    return cruces

-- mutacion Gaussiana

-- Hay que introducir una lista de individuos

--mutacion_gaussiana :: [[a]] -> [[a]]

-- distribucion_gaussiana x mu de = exp (-((x - mu)^2 / (2*de*de))) / sqrt (2*de*de*pi)


            
mutaciones_gaussianas [] = do 
    return []
mutaciones_gaussianas (xs:xss) =do
    mutacion <- mutacion_gaussiana xs
    resto <- mutaciones_gaussianas xss
    return (mutacion:resto)

mutacion_gaussiana [] = do
    return []
mutacion_gaussiana (x:xs) = do 
    rnd <-generaIndividuo 2
    let res = comprobacion_gauss x rnd
    resto <- mutacion_gaussiana xs
    return (res:resto)

comprobacion_gauss x rnd = if ((rnd!!0)<=1/30) then gauss else x
    where gauss = x + (distribucion_normal (rnd!!1))
    
-- Distribucion gaussiana con los valores mu 0 y sigma 1/20
distribucion_normal x = 20*(exp(-200*x^2)/sqrt(2*pi))

actualiza_poblacion poblacion eval_poblacion mutaciones eval_mutaciones subproblemas [] pesos z i = (poblacion,eval_poblacion)

actualiza_poblacion poblacion eval_poblacion mutaciones eval_mutaciones subproblemas vecindario@(vs:vss) pesos z i = 
    actualiza_poblacion pobl_act eval_pobl_act mutaciones eval_mutaciones subproblemas vss pesos z (i+1)
    where (pobl_act,eval_pobl_act)=actualiza_aux poblacion eval_poblacion mutaciones eval_mutaciones subproblemas vs pesos z i

actualiza_aux poblacion eval_poblacion _ _ _ [] _ _ _ = (poblacion,eval_poblacion)
actualiza_aux poblacion eval_poblacion mutaciones eval_mutaciones subproblemas (v:vs) pesos z i = (actualiza_aux pobl_act eval_pobl_act mutaciones eval_mutaciones subproblemas vs pesos z i)
    where (r1,r2) = (abs ((eval_mutaciones!!i)!1 - z!!0),abs ((eval_mutaciones!!i)!2 - z!!1))
          producto = [(fst (pesos!!i)) * r1, (snd (pesos!!i)) * r2]
          (pobl_act,eval_pobl_act) = 
              if (maximum producto) < (subproblemas!!v) 
                  then ((take v poblacion ++ [mutaciones!!i] ++ drop (v + 1) poblacion),take v eval_poblacion ++ [eval_mutaciones!!i] ++ drop (v + 1) eval_poblacion) 
                  else (poblacion,eval_poblacion)


algoritmo_agregacion_ZDT3 n g t f cr minimo maximo = do
    let pesos = calc_pesos (fromIntegral n)
    let vecindario = calc_vecindario pesos n t
    poblacion <- generaPoblacion n
    let eval_poblacion = evaluaciones poblacion
    let z = calc_z eval_poblacion
    res <-algoritmo_agregacion_ZDT3_aux poblacion eval_poblacion vecindario pesos z f cr minimo maximo g
    return res

algoritmo_agregacion_ZDT3_aux _ _ _ _ _ _ _ _ _ 0 = do
    return []
    
algoritmo_agregacion_ZDT3_aux poblacion eval_poblacion vecindario pesos z f cr minimo maximo g_fin = do
    mutantes <- calc_mutante vecindario poblacion f cr minimo maximo
    let eval_mutantes = evaluaciones mutantes
    let z_mutantes = calc_z eval_mutantes
    let z_act = [if ((z!!x)<(z_mutantes!!x)) then z!!x else z_mutantes!!x | x <- [0..2]]
    let subproblemas = calc_subproblemas eval_poblacion pesos z_act
    let (poblacion_act,eval_poblacion_act) = actualiza_poblacion poblacion eval_poblacion mutantes eval_mutantes subproblemas vecindario pesos z_act 0
    let res = (poblacion_act,eval_poblacion_act)
    resto <-algoritmo_agregacion_ZDT3_aux poblacion_act eval_poblacion_act vecindario pesos z_act f cr minimo maximo (g_fin-1)
    return (res:resto)
    
    
-- intento de main

{-- 
    let z_act = [if ((z!!x)<(z_mutantes!!x)) then z!!x else z_mutantes!!x | x <- [0..2]]
    
main n g t f cr min max = do
    let pesos = calc_pesos n
    let vecindario = calc_vecindario pesos n t
    poblacion <- generaPoblacion n
    let eval = evaluaciones poblacion
    let z = calc_z eval
    fin <- main_aux pesos vecindario poblacion eval z g f cr min max
    return fin
main_aux pesos vecindario poblacion eval z g f cr min max = undefined
--}


-- INSTRUCCIONES INICIALES
--     let p = calc_pesos 20
--     let vecindario = calc_vecindario p 20 0.3
--     poblacion <- generaPoblacion 20
--     let eval = evaluaciones poblacion
--     let z = calc_z eval

-- INTRUCCIONES TRIAL_VECTORS
--     calc_mutante vecindario poblacion 0.5 0.5 0 1
