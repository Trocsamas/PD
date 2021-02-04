module Funciones.Zdt3
(zdt3,
listaVector,
Vector
) where

import Data.Array

type Vector a = Array Int a

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