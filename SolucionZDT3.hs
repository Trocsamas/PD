module SolucionZDT3
    (SolucionZDT3,
     nuevaSolucionZDT3,
     generacionZDT3,
     selecciona_individuos_de_generacionZDT3,
     selecciona_evaluaciones_de_generacionZDT3
    ) where

import Data.Array

-- Definición de solucion
newtype SolucionZDT3 = S [([[Double]], [Array Int Double])]
    deriving Show
-- Crea tipo
nuevaSolucionZDT3 :: [([[Double]], [Array Int Double])] -> SolucionZDT3
nuevaSolucionZDT3 sol = S sol

-- Selecciona generación
generacionZDT3 :: Int -> SolucionZDT3 -> ([[Double]], [Array Int Double])
generacionZDT3 x (S sol) = sol!!(x-1)

-- Seleccionar individuos de una generación
selecciona_individuos_de_generacionZDT3 :: Int -> SolucionZDT3 -> [[Double]]
selecciona_individuos_de_generacionZDT3 x (S sol) = fst (sol!!(x-1))

-- Seleccionar evaluaciones de una generación
selecciona_evaluaciones_de_generacionZDT3 :: Int -> SolucionZDT3 -> [(Double, Double)]
selecciona_evaluaciones_de_generacionZDT3 x (S sol) = [((i!1),(i!2)) | i <- snd (sol!!(x-1))]
