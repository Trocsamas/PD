-- Algoritmo multiobjetivo basado en agregación sin restriccion

import Funciones.Zdt3
import Data.List
import Data.Array
import System.Random

-- Algoritmo de Agregación ZDT3 

algoritmo_agregacion_ZDT3 :: (Eq t, Num t) => Int -> t -> Double -> Double -> Double -> Double -> Double -> IO [([[Double]], [Array Int Double])]
algoritmo_agregacion_ZDT3 n g t f cr minimo maximo = do
    let pesos = calc_pesos (fromIntegral n)
    let vecindario = calc_vecindario pesos n t
    poblacion <- generaPoblacion n
    let eval_poblacion = evaluaciones poblacion
    let z = calc_z eval_poblacion
    res <- algoritmo_agregacion_ZDT3_aux poblacion eval_poblacion vecindario pesos z f cr minimo maximo g
    return res
algoritmo_agregacion_ZDT3_aux :: (Eq a, Num a) => [[Double]]-> [Array Int Double] -> [[Int]] -> [(Double, Double)] -> [Double] -> Double -> Double -> Double -> Double -> a -> IO [([[Double]], [Array Int Double])]
algoritmo_agregacion_ZDT3_aux _ _ _ _ _ _ _ _ _ 0 = do
    return []
algoritmo_agregacion_ZDT3_aux poblacion eval_poblacion vecindario pesos z f cr minimo maximo g_fin = do
    mutantes <- calc_mutante vecindario poblacion f cr minimo maximo
    let eval_mutantes = evaluaciones mutantes
    let z_mutantes = calc_z eval_mutantes
    let z_act = [if ((z!!x)<(z_mutantes!!x)) then z!!x else z_mutantes!!x | x <- [0..1]]
    let subproblemas = calc_subproblemas eval_poblacion pesos z_act
    let (poblacion_act,eval_poblacion_act) = actualiza_poblacion poblacion eval_poblacion mutantes eval_mutantes subproblemas vecindario pesos z_act 0
    let res = (poblacion_act,eval_poblacion_act)
    resto <-algoritmo_agregacion_ZDT3_aux poblacion_act eval_poblacion_act vecindario pesos z_act f cr minimo maximo (g_fin-1)
    return (res:resto)

-- -- Cálculo de los Vectores de Pesos

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

-- -- Cálculo del Vecindario

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

parte :: [a] -> Int -> [[a]]
parte [] _ = []
parte xs n = (take n xs) : parte (drop n xs) n 

-- -- Cálculo de la Población Inicial
-- =========================================================================
-- Para generar una población inicial, hemos hecho uso de la libreria 
-- System.Random, que genera números aleatorios tomando valores entre 0 y 1, 
-- que nosotros hemos agrupado en N listas de 30 valores.
-- =========================================================================

generaPoblacion :: Int -> IO [[Double]]
generaPoblacion n = do
    individuo <- generaIndividuo (30*n)
    return (parte individuo 30)

generaIndividuo :: Int -> IO [Double]
generaIndividuo n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
  return (take n xs)

-- -- Cálculo de las Evaluaciones con ZDT3

evaluaciones :: Floating a => [[a]] -> [Funciones.Zdt3.Vector a]
evaluaciones [] = []
evaluaciones (x:xss) = zdt3 x : evaluaciones xss 
    

-- -- Cálculo del Punto Z
    
calc_z :: (Ix i, Num i, Ord a) => [Array i a] -> [a]
calc_z xs = [f1,f2]
    where f1 = minimum [ x ! 1 | x <- xs]
          f2 = minimum [ x ! 2 | x <- xs]
    
-- -- Cálculo del vector Mutante

-- =========================================================================
-- Para cada vector/individuo se seleccionan 3 vectores/individuos vecinos y
-- aplicamos la fórmula: vi (G + 1) = xr1(G) + F ∗ (xr2 (G) − xr3(G))
-- F es un parámetro de mutación, el cual tendrá un valor de 0.5
-- =========================================================================

calc_mutante :: [[Int]] -> [[Double]] -> Double -> Double -> Double -> Double -> IO [[Double]]
calc_mutante vecindario poblacion f cr min max = do
    seleccion <- seleccion_aleatoria poblacion vecindario
    let mutantes = mutaciones seleccion f min max
    mutantes_cruzados <- evolucion_diferencial mutantes poblacion cr
    mutantes_gaussianos <- mutaciones_gaussianas mutantes_cruzados
    let mutantes_finales = limitador mutantes_gaussianos min max
    return mutantes_finales

-- -- -- Funciones para el Cálculo del Vector Mutante

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
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs
randomIndex :: [a] -> IO Int
randomIndex [] = error "Cannot select an element from an empty list."
randomIndex list = getStdRandom $ randomR (0, length list - 1)

-- -- -- -- Mutaciones dentro del intervalo [min,max]

mutaciones :: (Ord t, Num t) => [[[t]]] -> t -> t -> t -> [[t]]
mutaciones elegidos f min max = limitador [mutaciones_aux x f|x<-elegidos]  min max
mutaciones_aux :: Num a => [[a]] -> a -> [a]
mutaciones_aux (i0:i1:i2:_) f = zipWith (+) i0 [f*x| x<-(zipWith (-) i1 i2)]

limitador :: Ord t => [[t]] -> t -> t -> [[t]]
limitador [] _ _ = []
limitador (xs:xss) min max = [(limitador_aux x min max)| x<-xs]:limitador xss min max
limitador_aux :: Ord a => a -> a -> a -> a
limitador_aux x min max
    | x<min = min
    | x>max = max
    |otherwise = x

-- -- -- -- Cálculo de cruces con vector mutante

-- =========================================================================
-- Generamos una lista de True o False del tamaño de la dimensión del
-- problema, generando números aleatorios y comparando si el valor es menor
-- que el parámetro CR de cruce. Este parámetro también tendrá un valor de
-- 0.5.
-- Tras esto, para cada vector/individuo y su vector mutante, se seleccionan
-- los genes entre el vector/individuo y el vector mutante para conformar un
-- nuevo vector/individuo.
-- =========================================================================

evolucion_diferencial :: [[a]] -> [[a]] -> Double -> IO [[a]]
evolucion_diferencial [] [] _ = do
    return []
evolucion_diferencial mutantes@(x:xs) individuos@(y:ys) cr = do
    nuevo_individuo <- cruce_individuo x y cr
    resto <- evolucion_diferencial xs ys cr
    let cruzados = nuevo_individuo:resto
    return cruzados
cruce_individuo :: [a] -> [a] -> Double -> IO [a]
cruce_individuo mutante individuo cr = do
    cruces <- puntos_de_cruce cr
    let cruzados = [if cruces!!x then mutante!!x else individuo!!x | x <- [0..29]]
    return cruzados
puntos_de_cruce :: Double -> IO [Bool]
puntos_de_cruce cr = do
    individuo <- generaIndividuo 30
    let cruces = [x < cr | x <- individuo]
    return cruces

-- -- -- -- Mutacion Gaussianas

-- =========================================================================
-- Para introducir un poco más de diversidad en la búsqueda, aplicaremos
-- este tipo de mutación.La probabilidad de mutación de un gen será 1/p,
-- donde p es la dimensión del vector/individuo, y la desviación estándar 
-- σ =(XU − XL)/20
-- =========================================================================

mutaciones_gaussianas :: [[Double]] -> IO [[Double]]
mutaciones_gaussianas [] = do 
    return []
mutaciones_gaussianas (xs:xss) =do
    mutacion <- mutacion_gaussiana xs
    resto <- mutaciones_gaussianas xss
    return (mutacion:resto)
mutacion_gaussiana :: [Double] -> IO [Double]
mutacion_gaussiana [] = do
    return []
mutacion_gaussiana (x:xs) = do 
    rnd <-generaIndividuo 2
    let res = comprobacion_gauss x rnd
    resto <- mutacion_gaussiana xs
    return (res:resto)
comprobacion_gauss :: (Ord p, Floating p) => p -> [p] -> p
comprobacion_gauss x rnd = if ((rnd!!0)<=1/30) then gauss else x
    where gauss = x + (distribucion_normal (rnd!!1))
    

-- -- -- -- Distribucion gaussiana con los valores mu 0 y sigma 1/20

distribucion_normal :: Floating a => a -> a
distribucion_normal x = 20*(exp(-200*x^2)/sqrt(2*pi))


-- -- Cálculo de subproblemas
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

-- -- Actualizaciones de las poblaciones
actualiza_poblacion :: (Ix i, Num i, Num a1, Ord a1) => [a2] -> [Array i a1] -> [a2] -> [Array i a1] -> [a1] -> [[Int]] -> [(a1, a1)] -> [a1] -> Int -> ([a2], [Array i a1])
actualiza_poblacion poblacion eval_poblacion mutaciones eval_mutaciones subproblemas [] pesos z i = (poblacion,eval_poblacion)
actualiza_poblacion poblacion eval_poblacion mutaciones eval_mutaciones subproblemas vecindario@(vs:vss) pesos z i = 
    actualiza_poblacion pobl_act eval_pobl_act mutaciones eval_mutaciones subproblemas vss pesos z (i+1)
    where (pobl_act,eval_pobl_act)=actualiza_aux poblacion eval_poblacion mutaciones eval_mutaciones subproblemas vs pesos z i

actualiza_aux :: (Ix i, Num i, Num a1, Ord a1) => [a2] -> [Array i a1] -> [a2] -> [Array i a1] -> [a1] -> [Int] -> [(a1, a1)] -> [a1] -> Int -> ([a2], [Array i a1])
actualiza_aux poblacion eval_poblacion _ _ _ [] _ _ _ = (poblacion,eval_poblacion)
actualiza_aux poblacion eval_poblacion mutaciones eval_mutaciones subproblemas (v:vs) pesos z i = (actualiza_aux pobl_act eval_pobl_act mutaciones eval_mutaciones subproblemas vs pesos z i)
    where (r1,r2) = (abs ((eval_mutaciones!!i)!1 - z!!0),abs ((eval_mutaciones!!i)!2 - z!!1))
          producto = [(fst (pesos!!v)) * r1, (snd (pesos!!v)) * r2]
          (pobl_act,eval_pobl_act) = 
              if (maximum producto) < (subproblemas!!v)
                  then ((take v poblacion ++ [mutaciones!!i] ++ drop (v + 1) poblacion),(take v eval_poblacion ++ [eval_mutaciones!!i] ++ drop (v + 1) eval_poblacion)) 
                  else (poblacion,eval_poblacion)



