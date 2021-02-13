-- Aquí vamos a hacer el algoritmo de agregación con el manejo de restricciones

import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import Data.List
import Data.Array
import Funciones.Cf6
import System.Random


{--

--Algoritmo de Agregación con Restricciones mediante CF6

algoritmo_agregacion_restricciones n g t f cr = do
    let pesos = calc_pesos n
    let vecindario = calc_vecindario pesos n t
    poblacion <- generaPoblacion n 4
    (eval_poblacion,restricciones) = evalua_cf6 poblacion
    let z = calc_z eval_poblacion
    res <- algoritmo_agregacion_restricciones_aux poblacion eval_poblacion restricciones vecindario pesos z f cr g
    return res

algoritmo_agregacion_restricciones_aux poblacion eval_poblacion _ _ _ _ _ _ 0 = do 
    return []
algoritmo_agregacion_restricciones_aux poblacion eval_poblacion restricciones vecindario pesos z f cr g = do
    mutantes <- calc_mutantes_restricciones poblacion vecindario f cr
    (eval_mutantes,restricciones_mutantes) = evalua_cf6 mutantes
    z_mutante = calc_z eval_mutantes
    let z_act = [if ((z!!x)<(z_mutantes!!x)) then z!!x else z_mutantes!!x | x <- [0..2]]
    let subproblemas = calc_subproblemas eval_poblacion pesos z_act
    let (poblacion_act,eval_pobl_act,restricciones_act) = actualiza_poblacion_restricciones poblacion eval_poblacion restricciones eval_mutantes restricciones_mutantes subproblemas vecindario pesos z 0
    
    let res = (poblacion_act,eval_poblacion_act)
    resto <-algoritmo_agregacion_restricciones_aux poblacion_act eval_poblacion_act restricciones_act vecindario pesos z_act f cr (g_fin-1)
    return (res:resto)
    
--}

parte :: [a] -> Int -> [[a]]
parte [] _ = []
parte xs n = (take n xs) : parte (drop n xs) n

-- Cálculo de los vectores de pesos

calc_pesos :: (Enum b, Fractional b) => b -> [(b, b)]
calc_pesos n = [(0+paso*x,1-paso*x) | x <-[0..n-1]]
    where paso = 1/(n-1)

-- Cálculo del Vecindario

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
                    

-- Generación de las Población

generaPoblacion :: Int -> IO [[Double]]
generaPoblacion n = do
    individuo <- generaIndividuo (n*4)
    return (parte individuo 4)

generaIndividuo :: Int -> IO [Double]
generaIndividuo n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
  return (take n xs)

-- Evaluación de la Población mediante CF6 

evalua_cf6 [] = []
evalua_cf6 poblacion@(xs:xss)
    | xor (cumple ev2) = (ev1,[ev2!!0,ev2!!1,0]):evalua_cf6 xss
    | and (cumple ev2) = (ev1,[ev2!!0,ev2!!1,0]):evalua_cf6 xss
    | otherwise = (ev1,[ev2!!0,ev2!!1,0]):evalua_cf6 xss
    where (ev1,ev2) = cf6_4 xs

cumple = foldr (\x acc->(x<0):acc) []
xor xs = xs!!0 /= xs!!1 

-- Cálculo del Punto Z

calc_z xs = [f1,f2]
    where f1 = minimum [ x ! 1 | x <- xs]
          f2 = minimum [ x ! 2 | x <- xs]

-- Cálculo de los subproblemas

calc_subproblemas :: (Ix i, Num i, Num b, Ord b) => [Array i b] -> [(b, b)] -> [b] -> [b]
calc_subproblemas eval pesos z = maximo
    where resta = [((abs ((x!1)-z!!0),(abs ((x!2)-z!!1))))| x<-eval]
          maximo = calc_maximo (calc_subproblemas_aux resta pesos)

calc_subproblemas_aux :: (Num a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
calc_subproblemas_aux resta pesos = [((r1*p1),(r2*p2))| (r1,r2,p1,p2)<-xs]
    where (resta1,resta2) = unzip resta
          (pesos1,pesos2) = unzip pesos
          xs = zip4 resta1 resta2 pesos1 pesos2
          

calc_maximo :: Ord a => [(a, a)] -> [a]
calc_maximo [] = []
calc_maximo (x:xs) = max (fst x) (snd x) : calc_maximo xs

-- Cálculo del Vector Mutante con Restricciones

calc_mutantes_restricciones poblacion vecindario f cr = do
    seleccion <- seleccion_aleatoria poblacion vecindario
    let mutantes = mutaciones seleccion f (-2) 2
    let mutantes_limitados = limitador mutantes 0 1
    mutantes_cruzados <- evolucion_diferencial mutantes poblacion cr
    mutantes_gaussianos <- mutaciones_gaussianas mutantes_cruzados 
    let mutantes_gaussianos_limitados = limitador mutantes_gaussianos (-2) 2
    let mutantes_finales = limitador_primero mutantes_gaussianos_limitados
    return mutantes_finales
    -- Funciones para la aleatoriedad del cálculo del Vector Mutante

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
    
        
randomIndex :: [a] -> IO Int
randomIndex [] = error "Cannot select an element from an empty list."
randomIndex list = getStdRandom $ randomR (0, length list - 1)

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs
    -- mutaciones dentro del intervalo

mutaciones elegidos f min max = limitador [mutaciones_aux x f|x<-elegidos]  min max

limitador_primero [] = []
limitador_primero (xs:xss) = [(if (x == (xs!!0)) then (limitador_aux x 0 1) else x)| x<-xs]:limitador_primero xss

limitador [] _ _ = []
limitador (xs:xss) min max = [(limitador_aux x min max)| x<-xs]:limitador xss min max

limitador_aux x min max
    | x<min = min
    | x>max = max
    |otherwise = x

mutaciones_aux (i0:i1:i2:_) f = zipWith (+) i0 [f*x| x<-(zipWith (-) i1 i2)]

evolucion_diferencial :: [[a]] -> [[a]] -> Double -> IO [[a]]
evolucion_diferencial [] [] _ = do
    return []
evolucion_diferencial mutantes@(x:xs) individuos@(y:ys) cr = do
    nuevo_individuo <- cruce_individuo x y cr
    resto <- evolucion_diferencial xs ys cr
    let cruzados = nuevo_individuo:resto
    return cruzados
    
cruce_individuo mutante individuo cr = do
    cruces <- puntos_de_cruce cr
    let cruzados = [if cruces!!x then mutante!!x else individuo!!x | x <- [0..3]]
    return cruzados

puntos_de_cruce cr = do
    individuo <- generaIndividuo 4
    let cruces = [x < cr | x <- individuo]
    return cruces
    
mutaciones_gaussianas [] = do 
    return []
mutaciones_gaussianas (xs:xss) =do
    mutacion <- mutacion_gaussiana xs 0
    resto <- mutaciones_gaussianas xss
    return (mutacion:resto)

mutacion_gaussiana [] _ = do
    return []
mutacion_gaussiana (x:xs) i = do 
    rnd <-generaIndividuo 2
    let res = if (i == 0) then comprobacion_gauss x rnd (1/20) else comprobacion_gauss x rnd (1/5)
    resto <- mutacion_gaussiana xs (i+1)
    return (res:resto)

comprobacion_gauss x rnd sigma = if ((rnd!!0)<=1/4) then gauss else x
    where gauss = x + (distribucion_gaussiana (rnd!!1) 0 sigma)
    
-- Distribucion gaussiana 
distribucion_gaussiana x mu sigma = exp (-((x - mu)^2 / (2*sigma*sigma))) / sqrt (2*sigma*sigma*pi)
