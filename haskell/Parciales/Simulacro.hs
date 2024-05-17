module Simulacro where

-- ! Ejercicio 1
laTuplaPertenece :: (String, String) -> [(String, String)] -> Bool
laTuplaPertenece _ [] = False
laTuplaPertenece (n1, n2) ((x1, x2):xs)
    | n1 == x1 && n2 == x2 = True
    | n1 == x2 && n2 == x1 = True
    | otherwise = False

contieneTuplasRepetidas :: [(String, String)] -> Bool
contieneTuplasRepetidas [] = False
contieneTuplasRepetidas (x:xs)
    | laTuplaPertenece x xs = True
    | otherwise = contieneTuplasRepetidas xs

tieneTuplasConAmbosComponentesIguales :: [(String, String)] -> Bool
tieneTuplasConAmbosComponentesIguales [] = False
tieneTuplasConAmbosComponentesIguales ((x1, x2):xs)
    | x1 == x2 = True
    | otherwise = tieneTuplasConAmbosComponentesIguales xs

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas x = not (contieneTuplasRepetidas x) && not (tieneTuplasConAmbosComponentesIguales x)

-- ! Ejercicio 2
pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece n (x:xs)
    | n == x = True
    | otherwise = pertenece n xs

-- (x:xs) = Lista a analizar
-- ls = Lista a retornar
quitarRepetidosAux :: [String] -> [String] -> [String]
quitarRepetidosAux [] ls = ls
quitarRepetidosAux (x:xs) ls
    | pertenece x ls = quitarRepetidosAux xs ls
    | otherwise = quitarRepetidosAux xs (ls ++ [x])

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos [x] = [x]
quitarRepetidos x = quitarRepetidosAux x []

personasAux :: [(String, String)] -> [String]
personasAux [] = []
personasAux ((x1, x2):xs) = [x1, x2] ++ personasAux xs

-- [("asd", "dsa"), ("aaa", "bbb"), ("ddds", "bbb")] -> ["asd", "dsa", "aaa", "bbb", "ddds"]
personas :: [(String, String)] -> [String]
personas x = quitarRepetidos (personasAux x)

-- ! Ejercicio 3
-- José [("Julián", "José"), ("Pepe", "Roberto"), ("José", "Pablo")] -> ["Julián", "Pablo"]
amigosDeAux :: String -> [(String, String)] -> [String] -> [String]
amigosDeAux _ [] ls = ls
amigosDeAux s ((x1, x2):xs) ls
    | s == x1 = amigosDeAux s xs (ls ++ [x2])
    | s == x2 = amigosDeAux s xs (ls ++ [x1])
    | otherwise = amigosDeAux s xs ls

amigosDe :: String -> [(String, String)] -> [String]
amigosDe s x = amigosDeAux s x []

-- ! Ejercicio 4
longitud :: [String] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- x = Lista de personas
-- s = Persona con más amigos. Cambiará varias veces hasta que lleguemos al indicado
-- xor = x original
personaConMasAmigosAux :: [String] -> String ->  [(String, String)] -> String
personaConMasAmigosAux [] s _ = s
personaConMasAmigosAux (x:xs) s xor
    | longitud (amigosDe x xor) >= longitud (amigosDe s xor) = personaConMasAmigosAux xs x xor
    | otherwise = personaConMasAmigosAux xs s xor

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos x = personaConMasAmigosAux (personas x) "" x
