module Funciones.Zdt3
(zdt3,
listaVector,
solucion_de_pareto,
Vector
) where

import Data.Array

type Vector a = Array Int a

-- Solución: 8.528997e-01   -7.643075e-01
solucion_de_pareto :: Vector Double
solucion_de_pareto = zdt3 [8.528997e-01,4.455033e-04,2.023997e-03,5.397792e-03,5.944923e-04,1.067563e-03,3.566097e-03,1.382548e-03,6.985887e-04,1.958344e-04,
          1.076955e-03,1.207479e-03,9.879777e-03,1.368514e-04,9.025464e-04,5.268854e-04,4.552294e-03,1.122561e-05,1.755626e-03,3.247557e-04,
          7.025530e-03,1.466073e-03,2.234936e-05,1.498200e-03,1.303323e-04,1.820097e-03,8.807117e-06,1.728758e-03,5.288978e-04,1.224419e-03]


listaVector :: Num a => [a] -> Vector a
listaVector xs = listArray (1,n) xs
    where n = length xs

zdt3 :: Floating a => [a] -> Vector a
zdt3 var = listaVector [f1,f2]
    where f1 = var0
          f2 = g * (1 - sqrt (var0/g) - (var0/g) * sin (10*pi*var0))
          g  = 1+((9*tmp)/size)
          tmp = (sum . tail) var
          var0 = var !! 0
          size = fromIntegral (length var - 1)
