module SolucionCF6
    (SolucionCF6,
     nuevaSolucionCF6,
     generacionCF6,
     selecciona_individus_de_generacionCF6,
     selecciona_evaluaciones_de_generacionCF6
    ) where

import Data.Array

-- Definición de solucion
newtype SolucionCF6 = S [([[Double]], [Array Int Double], [[Double]])]

-- Crea tipo
nuevaSolucionCF6 sol = S sol

-- Selecciona generación
generacionCF6 x sol = sol!!(x-1)

-- Seleccionar individuos de una generación
selecciona_individus_de_generacionCF6 x sol = [((i!1),(i!2)) | i <- fst (sol!!(x-1))]

-- Seleccionar evaluaciones de una generación
selecciona_evaluaciones_de_generacionCF6 x sol = [((i!1),(i!2)) | i <- snd (sol!!(x-1))]