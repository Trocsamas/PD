-- Este fichero contendrá todas las funciones relacionadas 
-- con la lectura y escritura sobre ficheros
-- =======================================================


cargaDatos = do  
        let fileName = "./PF.dat"
        dat <- readFile fileName
        let lineas = lines dat
        return (aNumero (datos lineas)) 


split :: Eq a => a -> [a] -> [[a]]
split x y = func x y [[]]
    where
        func x [] z = reverse $ map (reverse) z
        func x (y:ys) (z:zs) = if y==x then 
            func x ys ([]:(z:zs)) 
        else 
            func x ys ((y:z):zs)

datos lineas = foldr (\palabra js-> split '\t' palabra:js) [] lineas

aNumero :: [[String]] -> [(Double, Double)]
aNumero [] = []
aNumero (x:xss) = (read (x!!0),read (x!!1)):aNumero xss


-- F por Windows
-- plotDots [Key Nothing, XRange (-0.1,1.1), YRange(-1.1,1.1)] a