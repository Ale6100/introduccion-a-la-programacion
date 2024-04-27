-- Ejercicio 1
-- Requiere que n >= 0
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- Ejercicio 2 (parteEntera)
-- A diferencia de lo que uno podría pensar, la idea de parteEntera no es eliminar el valor decimal, pero igual es lo que voy a hacer
parteEnteraPositivo :: Float -> Int
parteEnteraPositivo n
  | n < 1 = 0
  | otherwise = 1 + parteEnteraPositivo (n-1)

parteEnteraNegativo :: Float -> Int
parteEnteraNegativo n
  | n > -1 = 0
  | otherwise = -1 + parteEnteraNegativo (n+1)

parteEntera :: Float -> Int
parteEntera n
  | n > 0 = parteEnteraPositivo n
  | otherwise = parteEnteraNegativo n

-- Ejercicio 3
-- Determina si el primero es divisible por el segundo
esDivisible :: Int -> Int -> Bool
esDivisible divisor dividendo = parteDecimal == 0
  where
    division = (fromIntegral divisor :: Float) / (fromIntegral dividendo :: Float)
    parteEntera_ = parteEntera division
    parteDecimal = division - (fromIntegral parteEntera_ :: Float)

-- Ejercicio 4 (sumaImpares)
-- n debe ser natural
nEsimoImpar:: Int -> Int
nEsimoImpar n = 2*n - 1

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = nEsimoImpar n + sumaImpares(n-1)

-- Ejercicio 5 (medioFact)
-- n debe ser natural
-- Esto en realidad se conoce como "doble factorial"
medioFactAux :: Int -> Int -> Int
medioFactAux _ 0 = 1 -- i n
medioFactAux 0 n = n
medioFactAux i n = (n-2*i) * medioFactAux (i-1) n

medioFact :: Int -> Int
medioFact n = medioFactAux (div (n-1) 2) n

-- Ejercicio 6 (sumaDigitos)
-- n debe ser natural
cantidadDeCifrasAux :: Int -> Int -> Int
cantidadDeCifrasAux n i
  | n < 10 = i
  | otherwise = cantidadDeCifrasAux (div n 10) (i+1)

cantidadDeCifras :: Int -> Int
cantidadDeCifras n = cantidadDeCifrasAux n 1

ultimaCifra :: Int -> Int
ultimaCifra n = n - div n 10 * 10

sumaDigitosAux :: Int -> Int -> Int
sumaDigitosAux n 1 = n
sumaDigitosAux n r = ultimaCifra_ + sumaDigitosAux nDividido10 (r-1)
  where
    nDividido10 = div n 10
    ultimaCifra_ = n - nDividido10 * 10

sumaDigitos :: Int -> Int
sumaDigitos n = sumaDigitosAux n (cantidadDeCifras n)

-- Ejercicio 7
-- n debe ser natural
primeraCifra :: Int -> Int
primeraCifra n = div n (10^(cantidadDeCifras n - 1))

todosDigitosIguales :: Int -> Bool
todosDigitosIguales n
  | primeraCifra n /= ultimaCifra n = False
  | n < 10 = True
  | otherwise = todosDigitosIguales (div n 10)

-- Ejercicio 8
-- n debe ser natural o 0
-- i debe ser natural, menor o igual a la cantidad de dígitos de n
-- Literalmente en el enunciado está la respuesta
iesimoDigito :: Int -> Int -> Int
iesimoDigito n i = (n `div` 10^(cantidadDeCifras n - i)) `mod` 10

-- Ejercicio 9
-- n debe ser natural mayor a 0
-- esCapicua :: Int -> Bool
-- esCapicua n = esCapicuaAux n 0