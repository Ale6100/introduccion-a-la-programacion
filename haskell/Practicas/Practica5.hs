-- ! Ejercicio 1
-- a)
longitud :: [t] -> Int
longitud [] = 0
longitud [x] = 1
longitud (x:xs) = 1 + longitud xs

-- b)
-- La lista no debe estar vacía
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- c)
-- Debe retornar la misma lista pero sin el último elemento
-- La lista no debe estar vacía
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

-- d)
reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

-- ! Ejercicio 2
-- a)
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e [x] = e == x
pertenece e (x:xs)
  | e == x = True
  | otherwise = pertenece e xs

-- b)
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True -- Debatible
todosIguales [x] = True
todosIguales [x, y] = x == y
todosIguales (x:y:xs)
  | x == y = todosIguales (x:xs)
  | otherwise = False

-- c)
todosDistintosAux :: (Eq t) => t -> [t] -> Bool
todosDistintosAux _ [] = True
todosDistintosAux x (y:ys)
  | pertenece x (y:ys) = False
  | otherwise = todosDistintosAux y ys

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) = todosDistintosAux x xs

-- d)
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos x = not (todosDistintos x)

-- e)
-- n debe ser entero
-- Elimina la primera aparición de n en la lista (si está)
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n [x]
  | n == x = []
  | otherwise = [x]
quitar n (x:xs)
  | not (pertenece n (x:xs)) = x:xs
  | n == x = xs
  | otherwise = x : quitar n xs

-- f)
-- n debe ser entero
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos n x
  | pertenece n x = quitarTodos n (quitar n x)
  | otherwise = x

-- g)
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs)
  | not (hayRepetidos (x:xs)) = x:xs
  | otherwise = x : eliminarRepetidos (quitarTodos x xs)

-- h)
-- Las listas deben tener misma longitud
sonListasIguales :: (Eq t) => [t] -> [t] -> Bool
sonListasIguales [] [] = True
sonListasIguales [x] y = pertenece x y
sonListasIguales (x:xs) y
  | pertenece x y = sonListasIguales xs y
  | otherwise = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos x y
  | longitud x_ /= longitud y_ = False
  | otherwise = sonListasIguales x_ y_
  where
    x_ = eliminarRepetidos x
    y_ = eliminarRepetidos y

capicua :: (Eq t) => [t] -> Bool
capicua x = x == reverso x

-- ! Ejercicio 3
-- a)
-- Suma todos los elementos de la lista
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- b)
productoria :: [Int] -> Int
productoria [] = 0
productoria (x:xs)
  | xs == [] = x
  | otherwise = x * productoria xs

-- c)
-- Máximo elmento de la lista
-- La lista no debe ser vacía
maximo :: [Int] -> Int
maximo [x] = x
maximo [x, y]
  | x >= y = x
  | otherwise = y
maximo (x:y:xs)
  | x >= y = maximo (x:xs)
  | otherwise = maximo (y:xs)

-- d)
sumarN :: Int -> [Int] -> [Int]
sumarN 0 x = x
sumarN _ [] = []
sumarN n (x:xs) = (n+x) : sumarN n xs

-- e)
-- La lista no debe ser vacía
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- f)
-- La lista no debe ser vacía
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo x = sumarN (ultimo x) x

-- g)
esPar :: Int -> Bool
esPar n = mod n 2 == 0

pares :: [Int] -> [Int]
pares [] = []
pares [x]
  | esPar x = [x]
  | otherwise = []
pares (x:xs)
  | esPar x = x : pares xs
  | otherwise = pares xs

-- h)
esMultiploDeN :: Int -> Int -> Bool
esMultiploDeN n m = mod m n == 0

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN _ [] = []
multiplosDeN n [x]
  | esMultiploDeN n x = [x]
  | otherwise = []
multiplosDeN n (x:xs)
  | esMultiploDeN n x = x : multiplosDeN n xs
  | otherwise = multiplosDeN n xs

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar x = maximo_ : ordenar (quitar maximo_ x)
  where maximo_ = maximo x

-- ! Ejercicio 4
-- a)
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos [x, y]
  | x == y && x == ' ' = [x]
  | otherwise = [x, y]
sacarBlancosRepetidos (x:y:xs)
  | x == y && x == ' ' = sacarBlancosRepetidos (y:xs)
  | otherwise = x : sacarBlancosRepetidos (y:xs)

-- b)
-- El segundo parámetro es el contador que luego se retornará
contarPalabrasAux :: [Char] -> Int -> Int
contarPalabrasAux [] n = n-1
contarPalabrasAux (x:xs) i
  | x == ' ' = contarPalabrasAux xs (i+1)
  | otherwise = contarPalabrasAux xs i

asegurarEspacioEnLosExtremosYQuitarRepetidos :: [Char] -> [Char]
asegurarEspacioEnLosExtremosYQuitarRepetidos x = sacarBlancosRepetidos ( [' '] ++ x ++ [' '])

contarPalabras :: [Char] -> Int
contarPalabras [] = 0
contarPalabras x = contarPalabrasAux (asegurarEspacioEnLosExtremosYQuitarRepetidos x) 0

-- c)
-- x = Lista de carácteres sin blancos repetidos y con espacios en los extremos
-- p = Palabra que se está construyendo actualmente
-- li = Lista de palabras que se está construyendo
palabrasAux :: [Char] -> [Char] -> [[Char]] -> [[Char]]
palabrasAux (x:xs) p li
  | longitud (x:xs) == 1 && longitud p /= 0 = li ++ [p]
  | longitud (x:xs) == 1 && longitud p == 0 = li
  | x == ' ' && longitud p == 0 = palabrasAux xs [] li
  | x == ' ' && longitud p /= 0 = palabrasAux xs [] (li ++ [p])
  | otherwise = palabrasAux xs (p ++ [x]) li

palabras :: [Char] -> [[Char]]
palabras [] = []
palabras x = palabrasAux (asegurarEspacioEnLosExtremosYQuitarRepetidos x) [] []

-- d)
-- palabraMasLargaAux :: [[Char]] -> [Char] -> [Char]
-- palabraMasLargaAux []  = []

-- palabraMasLarga2 :: [Char]

-- palabraMasLarga :: [Char] -> [Char]
-- palabraMasLarga x = palabraMasLargaAux (palabras x) ' '
-- palabraMasLarga [] = []
-- palabraMasLarga = (x:xs)

-- iesimoElemento :: Int -> [[Char]] -> [Char]
-- iesimoElemento 


-- palabraMasLarga :: [Char] -> [Char]
-- palabraMasLarga [] = []
-- palabraMasLarga x = iesimoElemento i_ x_
--   where
--     x_ = palabras x
--     i_ = iesimaPalabraMasGrande x_