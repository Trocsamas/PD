module AgregacionCR
(algoritmo_agregacion_restricciones
) where

import Data.List
import Data.Array
import Funciones.Cf6
import System.Random


--Algoritmo de Agregación con Restricciones mediante CF6

algoritmo_agregacion_restricciones:: (Eq a, Num a) => Int -> a -> Double -> Double -> Double -> Int -> IO [([[Double]], [Array Int Double], [[Double]])]
algoritmo_agregacion_restricciones n g t f cr dimension = do
    let pesos = calc_pesos (fromIntegral n)
    let vecindario = calc_vecindario pesos n t
    poblacion <- generaPoblacion n dimension
    let eval_aux = unzip (evalua_cf6 poblacion dimension)
    let (eval_poblacion,restricciones) = (fst eval_aux, snd eval_aux)
    let z = calc_z eval_poblacion
    res <- algoritmo_agregacion_restricciones_aux poblacion eval_poblacion restricciones vecindario pesos z dimension f cr g
    return res

algoritmo_agregacion_restricciones_aux :: (Eq a, Num a) => [[Double]] -> [Array Int Double] -> [[Double]] -> [[Int]] -> [(Double, Double)] -> [Double] -> Int -> Double -> Double -> a -> IO [([[Double]], [Array Int Double], [[Double]])]
algoritmo_agregacion_restricciones_aux poblacion eval_poblacion _ _ _ _ _ _ _ 0 = do 
    return []
algoritmo_agregacion_restricciones_aux poblacion eval_poblacion restricciones vecindario pesos z dimension f cr g = do
    mutantes <- calc_mutantes_restricciones poblacion vecindario f cr dimension
    let eval_aux = unzip (evalua_cf6 mutantes dimension)
    let (eval_mutantes,restricciones_mutantes) = (fst eval_aux, snd eval_aux)
    let z_mutante = calc_z eval_mutantes
    let z_act = [if ((z!!x)<(z_mutante!!x)) then z!!x else z_mutante!!x | x <- [0..1]]
    let subproblemas = calc_subproblemas eval_poblacion pesos z_act
    let (poblacion_act,eval_poblacion_act,restricciones_act) = actualiza_poblacion_restricciones poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes subproblemas vecindario pesos z 0
    
    let res = (poblacion_act,eval_poblacion_act,restricciones_act)
    resto <-algoritmo_agregacion_restricciones_aux poblacion_act eval_poblacion_act restricciones_act vecindario pesos z_act dimension f cr (g-1)
    return (res:resto)


parte :: [a] -> Int -> [[a]]
parte [] _ = []
parte xs n = (take n xs) : parte (drop n xs) n

-- Cálculo de los vectores de pesos
-- =========================================================================
-- Se crea un conjunto de vectores peso de tamaño N), cumpliéndose que la 
-- suma de los componentes de cada vector es la unidad y dichos vectores 
-- peso están repartidos uniformemente.
-- Esto se consigue partiendo del vector (0,1) y calculando el paso a dar al 
-- siguiente vector para que quede uniformemente repartido paso = 1 / N−1
-- =========================================================================


calc_pesos :: (Enum b, Fractional b) => b -> [(b, b)]
calc_pesos n = [(0+paso*x,1-paso*x) | x <-[0..n-1]]
    where paso = 1/(n-1)

-- Cálculo del Vecindario

-- =========================================================================
-- Para cada vector/individuo se seleccionan 3 vectores/individuos vecinos y
-- aplicamos la fórmula:
-- vi (G + 1) = x r1(G) + F ∗ (xr2 (G) − xr3(G))
-- F es un parámetro de mutación, el cual tendrá un valor de 0.5
-- =========================================================================

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

-- =========================================================================
-- Para generar una población inicial, hemos hecho uso de la libreria 
-- System.Random, que genera números aleatorios tomando valores entre 0 y 1, 
-- que nosotros hemos agrupado en N listas de M valores.
-- =========================================================================

generaPoblacion :: Int -> Int -> IO [[Double]]
generaPoblacion n m = do
    individuo <- generaIndividuo (n*m)
    return (parte individuo m)

generaIndividuo :: Int -> IO [Double]
generaIndividuo n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
  return (take n xs)

-- Evaluación de la Población mediante CF6 

evalua_cf6 :: [[Double]] -> Int -> [(Funciones.Cf6.Vector Double, [Double])]
evalua_cf6 [] dimension = []
evalua_cf6 poblacion@(xs:xss) dimension 
    | xor (cumple ev2) = (ev1,[ev2!!0,ev2!!1,1]):evalua_cf6 xss dimension
    | and (cumple ev2) = (ev1,[ev2!!0,ev2!!1,0]):evalua_cf6 xss dimension
    | otherwise = (ev1,[ev2!!0,ev2!!1,2]):evalua_cf6 xss dimension
    where (ev1,ev2) = cf6 xs dimension

cumple :: [Double] -> [Bool]
cumple = foldr (\x acc->(x<0):acc) []
xor xs = xs!!0 /= xs!!1 

-- Cálculo del Punto Z

calc_z :: (Ix i, Num i, Ord a) => [Array i a] -> [a]
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

calc_mutantes_restricciones :: [[Double]] -> [[Int]] -> Double -> Double -> Int -> IO [[Double]]
calc_mutantes_restricciones poblacion vecindario f cr dimension= do
    seleccion <- seleccion_aleatoria poblacion vecindario
    let mutantes = mutaciones seleccion f (-2) 2
    let mutantes_limitados = limitador mutantes 0 1
    mutantes_cruzados <- evolucion_diferencial mutantes poblacion cr dimension
    mutantes_gaussianos <- mutaciones_gaussianas mutantes_cruzados (fromIntegral dimension)
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

mutaciones :: (Ord a, Num a) => [[[a]]] -> a -> a -> a -> [[a]]
mutaciones elegidos f min max = limitador [mutaciones_aux x f|x<-elegidos]  min max

limitador_primero :: (Ord a, Num a) => [[a]] -> [[a]]
limitador_primero [] = []
limitador_primero (xs:xss) = [(if (x == (xs!!0)) then (limitador_aux x 0 1) else x)| x<-xs]:limitador_primero xss

limitador :: Ord a => [[a]] -> a -> a -> [[a]]
limitador [] _ _ = []
limitador (xs:xss) min max = [(limitador_aux x min max)| x<-xs]:limitador xss min max

limitador_aux :: Ord a => a -> a -> a -> a
limitador_aux x min max
    | x<min = min
    | x>max = max
    |otherwise = x

mutaciones_aux :: Num a => [[a]] -> a -> [a]
mutaciones_aux (i0:i1:i2:_) f = zipWith (+) i0 [f*x| x<-(zipWith (-) i1 i2)]

evolucion_diferencial :: [[a]] -> [[a]] -> Double -> Int -> IO [[a]]
evolucion_diferencial [] [] _ _ = do
    return []
evolucion_diferencial mutantes@(x:xs) individuos@(y:ys) cr dimension = do
    nuevo_individuo <- cruce_individuo x y cr dimension
    resto <- evolucion_diferencial xs ys cr dimension
    let cruzados = nuevo_individuo:resto
    return cruzados

cruce_individuo :: [a] -> [a] -> Double -> Int -> IO [a]
cruce_individuo mutante individuo cr dimension = do
    cruces <- puntos_de_cruce cr dimension
    let cruzados = [if cruces!!x then mutante!!x else individuo!!x | x <- [0..(dimension-1)]]
    return cruzados

puntos_de_cruce :: Double -> Int -> IO [Bool]
puntos_de_cruce cr dimension= do
    individuo <- generaIndividuo dimension
    let cruces = [x < cr | x <- individuo]
    return cruces
    
mutaciones_gaussianas :: [[Double]] -> Double -> IO [[Double]]
mutaciones_gaussianas [] _ = do 
    return []
mutaciones_gaussianas (xs:xss) dimension =do
    mutacion <- mutacion_gaussiana xs 0 dimension
    resto <- mutaciones_gaussianas xss dimension
    return (mutacion:resto)

mutacion_gaussiana :: (Eq a, Num a) => [Double] -> a -> Double -> IO [Double]
mutacion_gaussiana [] _ _ = do
    return []
mutacion_gaussiana (x:xs) i dimension = do 
    rnd <-generaIndividuo 2
    let res = if (i == 0) then comprobacion_gauss x rnd (1/20) dimension else comprobacion_gauss x rnd (1/5) dimension
    resto <- mutacion_gaussiana xs (i+1) dimension
    return (res:resto)
    
comprobacion_gauss :: (Ord p, Floating p) => p -> [p] -> p -> p -> p
comprobacion_gauss x rnd sigma dimension = if ((rnd!!0)<=1/dimension) then gauss else x
    where gauss = x + (distribucion_gaussiana (rnd!!1) 0 sigma)
    

        -- Distribucion gaussiana 
distribucion_gaussiana :: Floating a => a -> a -> a -> a
distribucion_gaussiana x mu sigma = exp (-((x - mu)^2 / (2*sigma*sigma))) / sqrt (2*sigma*sigma*pi)

-- Actualizacion de las Poblaciones, Evaluaciones y Restricciones


actualiza_poblacion_restricciones :: (Ix i, Num a1, Num i, Num a2, Ord a2, Ord a1) => [a3] -> [Array i a2] -> [[a1]] -> [a3] -> [Array i a2] -> [[a1]] -> [a2] -> [[Int]] -> [(a2, a2)] -> [a2] -> Int -> ([a3], [Array i a2], [[a1]])
actualiza_poblacion_restricciones poblacion eval_poblacion restricciones _ _ _ _ [] _ _ _ = (poblacion,eval_poblacion,restricciones)
actualiza_poblacion_restricciones poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes subproblemas vecindario@(vs:vss) pesos z i = actualiza_poblacion_restricciones poblacion_act eval_poblacion_act restricciones_act mutantes eval_mutantes restricciones_mutantes subproblemas vss pesos z (i+1)
    where (poblacion_act,eval_poblacion_act,restricciones_act) = actualiza_poblacion_restricciones_aux poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes subproblemas vs pesos z i

actualiza_poblacion_restricciones_aux :: (Ix i, Num a1, Num i, Num a2, Ord a2, Ord a1) => [a3] -> [Array i a2] -> [[a1]] -> [a3] -> [Array i a2] -> [[a1]] -> [a2] -> [Int] -> [(a2, a2)] -> [a2] -> Int -> ([a3], [Array i a2], [[a1]])
actualiza_poblacion_restricciones_aux poblacion eval_poblacion restricciones _ _ _ _ [] _ _ _ = (poblacion,eval_poblacion,restricciones)
    
actualiza_poblacion_restricciones_aux poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes subproblemas vecinos@(v:vs) pesos z i = actualiza_poblacion_restricciones_aux poblacion_act eval_pobl_act restricciones_act mutantes eval_mutantes restricciones_mutantes subproblemas vs pesos z i
    where (rest_v,rest_mut_i) = ((restricciones!!v)!!2,(restricciones_mutantes!!i)!!2)
          (poblacion_act,eval_pobl_act,restricciones_act) = if (and [(rest_v==rest_mut_i),(rest_v==0)]) then (caso1 poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes subproblemas pesos z v i) else if (and [(rest_mut_i>0),(rest_v>0)]) then (caso2 poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes v i) else if (and [(rest_mut_i==0),(rest_v>0)]) then (caso3 poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes v i) else (poblacion,eval_poblacion,restricciones)

caso1 :: (Ix i, Num i, Num a1, Ord a1) => [a2] -> [Array i a1] -> [a3] -> [a2] -> [Array i a1] -> [a3] -> [a1] -> [(a1, a1)] -> [a1] -> Int -> Int -> ([a2], [Array i a1], [a3])
caso1 poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes subproblemas pesos z v i= (poblacion_act,eval_poblacion_act,restricciones_act)
    where (r1,r2) = (abs ((eval_mutantes!!i)!1 - z!!0),abs ((eval_mutantes!!i)!2 - z!!1))
          producto = [(fst (pesos!!v)) * r1, (snd (pesos!!v)) * r2]
          (poblacion_act,eval_poblacion_act,restricciones_act) = 
              if (maximum producto) < (subproblemas!!v)
                  then ((take v poblacion ++ [mutantes!!i] ++ drop (v + 1) poblacion),(take v eval_poblacion ++ [eval_mutantes!!i] ++ drop (v + 1) eval_poblacion),(take v restricciones ++ [restricciones_mutantes!!i] ++ drop (v + 1) restricciones)) 
                  else (poblacion,eval_poblacion,restricciones)
                
caso2 :: (Num a1, Ord a1) => [a2] -> [a3] -> [[a1]] -> [a2] -> [a3] -> [[a1]] -> Int -> Int -> ([a2], [a3], [[a1]])       
caso2 poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes v i= (poblacion_act,eval_poblacion_act,restricciones_act)
    where error_hijo = sum [((restricciones!!v)!!x)|x<-[0,1],((restricciones!!v)!!x)<0] 
          error_padre = sum [((restricciones_mutantes!!v)!!x)|x<-[0,1],((restricciones_mutantes!!v)!!x)<0]
          (poblacion_act,eval_poblacion_act,restricciones_act) = 
              if error_hijo > error_padre
                  then ((take v poblacion ++ [mutantes!!i] ++ drop (v + 1) poblacion),(take v eval_poblacion ++ [eval_mutantes!!i] ++ drop (v + 1) eval_poblacion),(take v restricciones ++ [restricciones_mutantes!!i] ++ drop (v + 1) restricciones)) 
                  else (poblacion,eval_poblacion,restricciones)
 
caso3 :: [a1] -> [a2] -> [a3] -> [a1] -> [a2] -> [a3] -> Int -> Int -> ([a1], [a2], [a3])
caso3 poblacion eval_poblacion restricciones mutantes eval_mutantes restricciones_mutantes v i= (poblacion_act,eval_poblacion_act,restricciones_act)
    where (poblacion_act,eval_poblacion_act,restricciones_act) = if (v == i)
           then ((take v poblacion ++ [mutantes!!i] ++ drop (v + 1) poblacion),(take v eval_poblacion ++ [eval_mutantes!!i] ++ drop (v + 1) eval_poblacion),(take v restricciones ++ [restricciones_mutantes!!i] ++ drop (v + 1) restricciones)) 
           else (poblacion,eval_poblacion,restricciones)
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
