-- Ejercicio 1
-- Requiere que n >= 0
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- Ejercicio 2
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

-- Ejercicio 4
-- n debe ser natural
nEsimoImpar:: Int -> Int
nEsimoImpar n = 2*n - 1

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = nEsimoImpar n + sumaImpares(n-1)

-- Ejercicio 5
-- n debe ser natural
-- Esto en realidad se conoce como "doble factorial"
medioFactAux :: Int -> Int -> Int
medioFactAux _ 0 = 1 -- i n
medioFactAux 0 n = n
medioFactAux i n = (n-2*i) * medioFactAux (i-1) n

medioFact :: Int -> Int
medioFact n = medioFactAux (div (n-1) 2) n
