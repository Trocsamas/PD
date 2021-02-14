module SolucionZDT3
    (SolucionZDT3,
     nuevaSolucionZDT3,
     generacionZDT3,
     selecciona_individus_de_generacionZDT3,
     selecciona_evaluaciones_de_generacionZDT3
    ) where

import Data.Array

-- Definición de solucion
newtype SolucionZDT3 = S [([[Double]], [Array Int Double])]

-- Crea tipo
nuevaSolucionZDT3 sol = S sol

-- Selecciona generación
generacionZDT3 x sol = sol!!(x-1)

-- Seleccionar individuos de una generación
selecciona_individus_de_generacionZDT3 x sol = [((i!1),(i!2)) | i <- fst (sol!!(x-1))]

-- Seleccionar evaluaciones de una generación
selecciona_evaluaciones_de_generacionZDT3 x sol = [((i!1),(i!2)) | i <- snd (sol!!(x-1))]