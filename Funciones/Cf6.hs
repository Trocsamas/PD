module Funciones.Cf6
(cf6
) where

import Data.Array

type Vector a = Array Int a

listaVector :: Num a => [a] -> Vector a
listaVector xs = listArray (1,n) xs
    where n = length xs

cf6 :: (Ord a, Floating a) => [a] -> Int -> (Vector a,[a])
cf6 xs dimension= (listaVector [f1,f2],[restriccion1,restriccion2])
    where f1    = x0 + sum1
          f2    = (1 - x0) * (1 - x0) + sum2
          sum1  = sum [ yj x * yj x | x <- [2..dimension],  (mod x 2) == 1]
          sum2  = sum [ yi x * yi x | x <- [2..dimension],  (mod x 2) /= 1]
          yj j  = (xs!!(j-1)) - 0.8 * x0 * cos (6*pi*x0+(fromIntegral j)*pi/(fromIntegral dimension))
          yi i  = (xs!!(i-1)) - 0.8 * x0 * sin (6*pi*x0+(fromIntegral i)*pi/(fromIntegral dimension))
          restriccion1 = x1 - 0.8 * x0 * sin (6*x0*pi+2.0*pi/(fromIntegral dimension)) -
                         sgn ((x0-0.5) * (1-x0)) * sqrt (abs ((x0-0.5) * (1.0-x0)))
          restriccion2 = x3 - 0.8 * x0 * sin (6*x0*pi+4.0*pi/(fromIntegral dimension)) -
                         sgn (0.25 * sqrt (1-x0) - 0.5 * (1-x0)) *
                         sqrt (abs (0.25 * sqrt (1-x0) - 0.5 * (1-x0)))
          x0 = xs !! 0
          x1 = xs !! 1
          x3 = xs !! 3

sgn :: (Ord a, Fractional p, Num a) => a -> p
sgn x 
    | x > 0 = 1.0
    | x < 0 = (-1.0)
    | otherwise = 0.0
