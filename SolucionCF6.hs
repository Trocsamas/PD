module SolucionCF6
    (SolucionCF6,
     nuevaSolucionCF6,
     generacionCF6,
     selecciona_individuos_de_generacionCF6,
     selecciona_evaluaciones_de_generacionCF6,
     selecciona_restricciones_de_generacionCF6
    ) where

import Data.Array

-- Definición de solucion
newtype SolucionCF6 = S [([[Double]], [Array Int Double], [[Double]])]
    deriving Show
-- Crea tipo
nuevaSolucionCF6 :: [([[Double]], [Array Int Double], [[Double]])] -> SolucionCF6
nuevaSolucionCF6 sol = S sol

-- Selecciona generación
generacionCF6 :: Int -> SolucionCF6 -> ([[Double]], [Array Int Double], [[Double]])
generacionCF6 x (S sol) = sol!!(x-1)

-- Seleccionar individuos de una generación
selecciona_individuos_de_generacionCF6 :: Int -> SolucionCF6 -> [[Double]]
selecciona_individuos_de_generacionCF6 x (S sol) = fst' (sol!!(x-1))

-- Seleccionar evaluaciones de una generación
selecciona_evaluaciones_de_generacionCF6 :: Int -> SolucionCF6 -> [(Double, Double)]
selecciona_evaluaciones_de_generacionCF6 x (S sol) = [((i!1),(i!2)) | i <- snd' (sol!!(x-1))]

-- Selecciona restriciones de una generación
selecciona_restricciones_de_generacionCF6 :: Int -> SolucionCF6 -> [[Double]]
selecciona_restricciones_de_generacionCF6 x (S sol) = thd' (sol!!(x-1))

fst' :: (a, b, c) -> a
fst' (x,_,_) = x

snd' :: (a, b, c) -> b
snd' (_,y,_) = y

thd' :: (a, b, c) -> c
thd' (_,_,z) = z
