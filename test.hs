doubleMe x = x + x

f :: Int -> Int
f 1 = 8 -- f(1) = 8
f 4 = 131
f 16 = 16

g :: Int -> Int
g 8 = 16
g 16 = 4
g 131 = 1

h :: Int -> Int
h x = f (g x)

k :: Int -> Int
k x = g (f x)

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z
    | x == y = x+z
    | x == z = x+y
    | y == z = x+y
    | otherwise = x + y + z

digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor t1 t2
    | fst t1 < fst t2 && snd t1 < snd t2 = True
    | otherwise = False
-- -todoMenor (x, y) (z, w) = x < z && y < w -- Alternativa dada por IA
