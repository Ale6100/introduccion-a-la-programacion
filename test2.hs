todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor t1 t2
    | fst t1 < fst t2 && snd t1 < snd t2 = True
    | otherwise = False

todoMenor2 :: (Float, Float) -> (Float, Float) -> Bool
todoMenor2 (x, y) (z, w) = x < z && y < w

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (x, y, z)
    | even x = 1 -- Consideramos que la primer posición es 1 | mod x 2 == 0 es equivalente a even x
    | even y = 2
    | even z = 3
    | otherwise = 4

-- biciesto:: Int -> Bool
-- biciesto anio
-- (mod anio 4 == 0 || (mod anio 100 /= 0 && mod anio 400 == 0)) = False
--  | otherwise = True

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x1, y1, z1) (x2, y2, z2) = ((x1 - x2)**2)*(1/2) + ((y1 - y2)**2)*(1/2) + ((z1 - z2)**2)*(1/2)

ultimoDigitoDeA :: Int -> Int
ultimoDigitoDeA a = mod a 10

anteUltimoDigitoDeA :: Int -> Int
anteUltimoDigitoDeA a = mod (div a 10) 10

sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos n = ultimoDigitoDeA n + anteUltimoDigitoDeA n

comparar :: Int -> Int -> Int
comparar a b
    | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
    | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
    | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0
