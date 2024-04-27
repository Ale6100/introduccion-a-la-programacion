-- ! Ejercicio 1
-- Requiere que n >= 0
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- ! Ejercicio 2 (parteEntera)
parteEnteraPositivo :: Float -> Int
parteEnteraPositivo n
  | n < 1 = 0
  | otherwise = 1 + parteEnteraPositivo (n-1)

parteEnteraNegativo :: Float -> Int
parteEnteraNegativo n
  | n > -1 = 0
  | otherwise = -1 + parteEnteraNegativo (n+1)

-- A diferencia de lo que uno podría pensar, la idea de parteEntera no es eliminar el valor decimal, pero igual es lo que voy a hacer
parteEntera :: Float -> Int
parteEntera n
  | n > 0 = parteEnteraPositivo n
  | otherwise = parteEnteraNegativo n

-- ! Ejercicio 3
-- Determina si el primero es divisible por el segundo
esDivisible :: Int -> Int -> Bool
esDivisible divisor dividendo = parteDecimal == 0
  where
    division = (fromIntegral divisor :: Float) / (fromIntegral dividendo :: Float)
    parteEntera_ = parteEntera division
    parteDecimal = division - (fromIntegral parteEntera_ :: Float)

-- ! Ejercicio 4 (sumaImpares)
nEsimoImpar:: Int -> Int
nEsimoImpar n = 2*n - 1

-- n debe ser natural
sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = nEsimoImpar n + sumaImpares(n-1)

-- ! Ejercicio 5 (medioFact)
medioFactAux :: Int -> Int -> Int
medioFactAux _ 0 = 1 -- i n
medioFactAux 0 n = n
medioFactAux i n = (n-2*i) * medioFactAux (i-1) n

-- n debe ser natural
-- Esto en realidad se conoce como "doble factorial"
medioFact :: Int -> Int
medioFact n = medioFactAux (div (n-1) 2) n

-- ! Ejercicio 6 (sumaDigitos)
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

-- n debe ser natural
sumaDigitos :: Int -> Int
sumaDigitos n = sumaDigitosAux n (cantidadDeCifras n)

-- ! Ejercicio 7 (todosDigitosIguales)
primeraCifra :: Int -> Int
primeraCifra n = div n (10^(cantidadDeCifras n - 1))

-- n debe ser natural
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n
  | primeraCifra n /= ultimaCifra n = False
  | n < 10 = True
  | otherwise = todosDigitosIguales (div n 10)

-- ! Ejercicio 8
-- n debe ser natural o 0
-- i debe ser natural, menor o igual a la cantidad de dígitos de n
-- Literalmente en el enunciado está la respuesta
iesimoDigito :: Int -> Int -> Int
iesimoDigito n i = (n `div` 10^(cantidadDeCifras n - i)) `mod` 10

-- ! Ejercicio 9 (esCapicua)
quitarPrimerDigito :: Int -> Int
quitarPrimerDigito n = n - div n m * m
  where m = 10^(cantidadDeCifras n -1)

-- n debe ser natural o 0
esCapicua :: Int -> Bool
esCapicua n
  | n < 10 = True
  | primeraCifra n /= ultimaCifra n = False
  | otherwise = esCapicua (quitarPrimerDigito (div n 10))

-- ! Ejercicio 10
-- n debe ser natural o cero
f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1(n-1)

-- n debe ser natural, q debe ser real
f2 :: Int -> Float -> Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q

-- n debe ser natural (aunque también pedía que pueda ser cero), q debe ser real
f3 :: Int -> Float -> Float
f3 1 q = q + q^2
f3 n q = q^(2*n) + f3 (n-1) q

-- n debe ser natural o cero, q debe ser real
f4Aux :: Int -> Float -> Int -> Float
f4Aux n q i
  | n == i = q^i
  | otherwise = q^i + f4Aux n q (i-1)

f4 :: Int -> Float -> Float
f4 n q = f4Aux n q (2*n)

-- ! Ejercicio 11
-- a)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

-- n debe ser natural o cero
eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (1/ fromIntegral (factorial n) :: Float) + eAprox(n-1)

-- b)
e = eAprox 9

-- ! Ejercicio 12
-- n debe ser natural
-- raizDe2Aprox :: Int -> Float
-- raizDe2Aprox 1 = 0
-- raizDe2Aprox n