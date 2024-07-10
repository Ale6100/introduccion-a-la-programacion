module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf

-- Completar!
-- Nombre de grupo: { Lambda }
-- Integrante1: { 42340456, Portaluppi Alejandro Martin, ale610013@gmail.com }
-- Integrante2: { 41929753, Alarcon Eros Roman, erosromanalarcon@gmail.com }
-- Integrante3: { 45480373, Schiaffino Facundo Miguel, facumiguel4025@gmail.com }
-- Integrante4: { 51398756, Krivonosoff Basualdo Thiago, thiagokribas@gmail.com }
-- Integrantes que abandonaron la materia: {}

-- ! EJ 1

-- Funcion auxiliar que comprueba si un elemento existe en una lista
pertenece :: (Eq dato) => dato -> [dato] -> Bool
pertenece _ [] = False
pertenece elem (x:xs)
    | elem == x = True
    | otherwise = pertenece elem xs

esMinuscula :: Char -> Bool
esMinuscula letra = pertenece letra "abcdefghijklmnopqrstuvwxyz"

-- ! EJ 2

-- Función que devuelve la posicion de una letra en un string
letraANaturalAux :: Char -> String -> Int
letraANaturalAux letra (x:xs)
    | letra == x = 0
    | otherwise = 1 + letraANaturalAux letra xs

-- "letra" es una letra minúscula, se envia la letra y el abecedario en minúsculas para comparar
letraANatural :: Char -> Int
letraANatural letra = letraANaturalAux letra "abcdefghijklmnopqrstuvwxyz"

-- ! EJ 3

-- Función que desplaza una letra minúscula n lugares en el abecedario
desplazar :: Char -> Int -> Char
desplazar letra 0 = letra
desplazar letra n
    | not (esMinuscula letra) = letra
    | otherwise = naturalALetra (n + letraANatural letra)

-- Opuesta de letraANatural, es auxiliar, ingresas un natural y devuelve la letra correspondiente
naturalALetra :: Int -> Char
naturalALetra n
    | n > 25 = naturalALetra (n - 26)
    | n < 0 = naturalALetra (n + 26)
    | otherwise = naturalALetraAux n "abcdefghijklmnopqrstuvwxyz"

naturalALetraAux :: Int -> String -> Char
naturalALetraAux n (x:xs)
    | n == letraANatural x = x
    | otherwise = naturalALetraAux n xs

-- ! EJ 4

cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n
    | not (esMinuscula x) = x : cifrar xs n
    | otherwise = desplazar x n : cifrar xs n

-- ! EJ 5

-- Como la función desplazar transforma los números negativos en el equivalente positivo -1 = 25, se puede introducir en negativo lo ingresado para desplazar hasta la clave original
descifrar :: String -> Int -> String
descifrar palabra n = cifrar palabra (-n)

-- ! EJ 6

-- Función que cifra una lista de strings
cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (x:xs) n = descifrar x n : cifrarListaAux xs (n-1)

cifrarLista :: [String] -> [String]
cifrarLista ls = cifrarListaAux ls 0

-- ! EJ 7

-- Función que calcula la frecuencia de cada letra del abecedario en una clave
frecuencia :: String -> [Float]
frecuencia [] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
frecuencia clave = frecuenciaAux "abcdefghijklmnopqrstuvwxyz" (sinEspacios clave)

-- Función auxiliar que calcula la fracuencia de cada letra de una palabra, en una clave
frecuenciaAux :: String -> String -> [Float]
frecuenciaAux [] _ = []
frecuenciaAux (letra:palabra) clave = porcentaje letra clave : frecuenciaAux palabra clave

-- Función que calcula el porcentaje de un elemento en una lista
porcentaje :: (Eq dato) => dato -> [dato] -> Float
porcentaje _ [] = 0
porcentaje elem lista = (fromIntegral (vecesAparece elem lista) * 100) / fromIntegral (longitud lista)

-- Función que elimina los espacios de una clave
sinEspacios :: String -> String
sinEspacios [] = []
sinEspacios (l:ls)
    | l == ' ' = sinEspacios ls
    | otherwise = l : sinEspacios ls

-- Función que devuelve la longitud de una lista
longitud :: [dato] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Función que devuelve las veces que aparece un elemento en una lista
vecesAparece :: (Eq dato) => dato -> [dato] -> Int
vecesAparece _ [] = 0
vecesAparece elem (x:xs)
    | x == elem = 1 + vecesAparece elem xs
    | otherwise = vecesAparece elem xs

-- ! Ej 8

-- Dado una clave, y una lista de porcentajes, devuelve la letra mas frecuente en la clave. (Ej: clave = ala, porcentaje = [66.666,33.333,66.666], res = (a, 66.666))
cifradoMasFrecuenteAux:: String -> [Float] -> (Char,Float)
cifradoMasFrecuenteAux [letra] [porcentaje] = (letra, porcentaje)
cifradoMasFrecuenteAux (letra1:letra2:clave) (porcentaje1:porcentaje2:porcentajes)
    | porcentaje1 >= porcentaje2 = cifradoMasFrecuenteAux (letra1:clave) (porcentaje1:porcentajes)
    | otherwise = cifradoMasFrecuenteAux (letra2:clave) (porcentaje2:porcentajes)

cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente clave n = cifradoMasFrecuenteAux codigo (frecuenciaAux codigo codigo)
    where codigo = cifrar clave n

-- ! EJ 9

-- Dados dos strings y un número, codifica el primer string con el número, y si ambos strings son iguales, devuelve Verdadero, si no, repite recursivamente sumando 1 al número hasta terminar el abecedario (z = 25)
esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux clave codigo 26 = False
esDescifradoAux clave codigo n
    | cifrar clave n == codigo = True
    | otherwise = esDescifradoAux clave codigo (n+1)

esDescifrado :: String -> String -> Bool
esDescifrado clave codigo
    | clave == codigo = True
    | otherwise = esDescifradoAux clave codigo 1

-- ! EJ 10

todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados lista = todosLosDescifradosAux lista lista

-- Función que, ingresadas dos listas, devuelve una lista de tuplas, donde el primer elemento de cada tupla pertenece a la primera lista y es cifrado del segundo elemento, que pertenece a la segunda lista
todosLosDescifradosAux :: [String] -> [String] -> [(String, String)]
todosLosDescifradosAux [] _ = []
todosLosDescifradosAux (elem1:lista) comparacion = todosLosDescifradosElem elem1 comparacion ++ todosLosDescifradosAux lista comparacion

-- Función que, ingresado un elemento y una lista, devuelve todas las tuplas donde el segundo elemento pertenece a la lista y es cifrado del primero
todosLosDescifradosElem :: String -> [String] -> [(String, String)]
todosLosDescifradosElem _ [] = []
todosLosDescifradosElem elem1 (elem2:recursion)
    | elem1 == elem2 = tdA elem1 recursion
    | esDescifrado elem1 elem2 = (elem1, elem2) : tdA elem1 recursion
    | otherwise = tdA elem1 recursion
    where tdA = todosLosDescifradosElem

-- ! EJ 11

-- Función que repite los caracteres de una clave hasta que tenga longitud n
expandirClave :: String -> Int -> String
expandirClave [] _ = []
expandirClave clave n = expandirClaveAux clave clave n

expandirClaveAux :: String -> String -> Int -> String
expandirClaveAux _ [] _ = []
expandirClaveAux [] clave n = expandirClaveAux clave clave n
expandirClaveAux _ _ 0 = []
expandirClaveAux (x:xs) clave n = x : expandirClaveAux xs clave (n-1)

-- ! EJ 12

-- Función auxiliar que transforma una palabra en una lista de sus respectivos valores numéricos (ej: "abcd" = [0,1,2,3])
palabraANatural:: String -> [Int]
palabraANatural [] = []
palabraANatural (x:xs) = letraANatural x : palabraANatural xs

-- Dado un código y una clave, cifra cada caracter del código desplazando el valor del caracter correspondiente de la clave
cifrarVigenere :: String -> String -> String
cifrarVigenere [] _ = []
cifrarVigenere codigo [] = codigo
cifrarVigenere codigo clave = cifrarVigenereAux codigo (palabraANatural (expandirClave clave (longitud codigo)))

-- Dados una palabra y una lista de int de misma longitud, desplaza una letra de la palabra, el numero de la lista en la misma posición que la letra
cifrarVigenereAux :: String -> [Int] -> String
cifrarVigenereAux [] _ = []
cifrarVigenereAux (x:palabra) (n:clave) = desplazar x n : cifrarVigenereAux palabra clave

-- ! EJ 13

-- Dados código y una clave, descifra cada caracter del código desplazando hacia atras el valor del caracter correspondiente de la clave
descifrarVigenere :: String -> String -> String
descifrarVigenere [] _ = []
descifrarVigenere codigo [] = codigo
descifrarVigenere codigo clave = descifrarVigenereAux codigo (palabraANatural (expandirClave clave (longitud codigo)))

-- Al ingresar una palabra y una lista de números de misma longitud, desplaza cada letra de palabra, el numero correspondiente en la lista, hacia atras.
descifrarVigenereAux :: String -> [Int] -> String
descifrarVigenereAux [] _ = []
descifrarVigenereAux (x:palabra) (n:clave) = desplazar x (-n) : descifrarVigenereAux palabra clave

-- ! EJ 14

--Funcion auxiliar que devuelve el valor absoluto
absoluto :: Int -> Int
absoluto x
    | x >= 0 = x
    | otherwise = -x

-- Función auxiliar que suma todos los números de una lista
sumaDeLista :: [Int] -> Int
sumaDeLista [] = 0
sumaDeLista (x:xs) = x + sumaDeLista xs

-- Dado un codigo y una lista de claves, devuelve la clave que menos desplazó al código
peorCifrado :: String -> [String] -> String
peorCifrado _ [clave] = clave
peorCifrado codigo (elem1:elem2:clave)
    | cDC (palabraANatural (cifrarVigenere codigo elem1)) <= cDC (palabraANatural (cifrarVigenere codigo elem2)) = peorCifrado codigo (elem1:clave)
    | otherwise = peorCifrado codigo (elem2:clave)
    where cDC = calcularDistanciaCifrado (palabraANatural codigo)

-- Dadas dos palabras de misma longitud devuelve la distancia entre sus valores
calcularDistanciaCifrado :: [Int] -> [Int] -> Int
calcularDistanciaCifrado [] _ = 0
calcularDistanciaCifrado _ [] = 0
calcularDistanciaCifrado (n:listaCodigo) (x:listaClave) = absoluto (n - x) + calcularDistanciaCifrado listaCodigo listaClave

-- ! EJ 15

-- Dados una palabra, una lista de claves, y un cifrado, compara si la palabra cifrada en esa clave es igual al cifrado, y entonces devuelve la lista de tuplas que cumplan
combinacionesVigenereAux :: String -> [String] -> String -> [(String, String)]
combinacionesVigenereAux _ [] _ = []
combinacionesVigenereAux palabra (x:claves) cifrado
    | cifrarVigenere palabra x == cifrado = (palabra, x) : combinacionesVigenereAux palabra claves cifrado
    | otherwise = combinacionesVigenereAux palabra claves cifrado


-- Dados, una lista de palabras, lista de claves, y un cifrado, devuelve una lista de tuplas donde el primer elemento pertenece a las palabras, y cifrado usando como clave el segundo que pertenece a las claves, es igual al cifrado original
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (x:palabra) claves cifrado
    | longitud x /= longitud cifrado = combinacionesVigenere palabra claves cifrado
    | otherwise = combinacionesVigenereAux x claves cifrado ++ combinacionesVigenere palabra claves cifrado
