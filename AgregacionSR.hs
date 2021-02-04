-- Algoritmo multiobjetivo basado en agregación sin restriccion

import Funciones.Zdt3
import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import Data.List

import System.Random

ej1 = zdt3 [8.528997e-01,4.455033e-04,2.023997e-03,5.397792e-03,5.944923e-04,1.067563e-03,3.566097e-03,1.382548e-03,6.985887e-04,1.958344e-04,
          1.076955e-03,1.207479e-03,9.879777e-03,1.368514e-04,9.025464e-04,5.268854e-04,4.552294e-03,1.122561e-05,1.755626e-03,3.247557e-04,
          7.025530e-03,1.466073e-03,2.234936e-05,1.498200e-03,1.303323e-04,1.820097e-03,8.807117e-06,1.728758e-03,5.288978e-04,1.224419e-03]

-- Solución: 8.528997e-01   -7.643075e-01

dib2 = 
    plotList [] (take 30 fibs)
    where fibs :: [Double] 
          fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- Función de redondeo

roundTo :: Double -> Int -> Double
roundTo x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n


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
             
-- Calculo de los vectores

calc_pesos n = [(0+paso*x,1-paso*x) | x <-[0..n-1]]
    where paso = 1/(n-1)

-- Calculo del punto Z

calc_z xss = calc_zaux (unzip xss)

calc_zaux (xs,ys) = [minimum xs, minimum ys]

-- Vencindario

distancia_euclidea :: Floating a => (a, a) -> (a, a) -> a
distancia_euclidea (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

parte :: [a] -> Int -> [[a]]
parte [] _ = []
parte xs n = (take n xs) : parte (drop n xs) n

distancias :: Floating a => [(a, a)] -> [[(a, Int)]]
distancias xs = parte [(distancia_euclidea i (xs !! j), j) | i <-xs, j <- [0..n-1]] n
    where n = (length xs)

calc_vecindario :: (Floating a1, Ord a1) => [(a1, a1)] -> Int -> Double -> [[Int]]
calc_vecindario xs n t = foldr (\xst ys -> f (take trunc (sort xst)):ys ) [] distss
    where distss = distancias xs
          trunc  = truncate (fromIntegral n*t)
          f yst  = foldr (\t js -> snd t :js) [] yst


-- Calculo de la Población Inicial

generaIndividuo :: Int -> IO [Double]
generaIndividuo n = do
  gen <- newStdGen
  let xs = randomRs (0,1) gen
  return (take n xs)

generaPoblacion n = do
    individuo <- generaIndividuo (30*n)
    return (parte individuo 30)
