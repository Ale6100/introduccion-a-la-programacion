-- Ejercicio 1
-- Requiere que n >= 0
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- Ejercicio 2
parteEnteraPositivo :: Float -> Int
parteEnteraPositivo n
  | n < 1 = 0
  | otherwise = 1 + parteEnteraPositivo (n-1)

parteEntera :: Float -> Int
parteEntera n
  | n > 0 = parteEnteraPositivo n
  | otherwise = parteEnteraNegativo n
