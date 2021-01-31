import Data.Array

type Vector a = Array Int a

listaVector :: Num a => [a] -> Vector a
listaVector xs = listArray (1,n) xs
    where n = length xs

cf6_4 :: Floating a => [a] -> Vector a
cf6_4 = undefined