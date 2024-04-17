-- Este archivo es horrible no lo leas

fibonacci :: Int -> Int
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci(n-1) + fibonacci(n-2)

parteDecimal2 :: Float -> Float
parteDecimal2 n
    | (n >= 0) && (n < 1) = n
    | otherwise = parteDecimal2(n-1)

parteEntera :: Float -> Float -- En realidad me pedían que retorne Int pero bueno xd
parteEntera n = n - parteDecimal2 n

{-
parteEntera :: Float -> Int
parteEntera x
    | ((n >= 0) && (n < 1)) = n
-}

{-
biciesto :: Int -> Bool
biciesto anio
    | (mod anio 4 /= 0 || (mod anio 100 == 0 && mod anio 400 /= 0)) = False
    | otherwise = True
-}

-- ultimoDigito :: Int -> Int
-- ultimoDigito n = mod n 10

-- nComaCorrida :: Int -> Int
-- nComaCorrida n = div n 10

-- suma3Digitos :: Int -> Int -- Para 3 dígitos
-- suma3Digitos n = ultimoDigito(n) + ultimoDigito(nComaCorrida(n)) + ultimoDigito(nComaCorrida(nComaCorrida(n)))

-- sumaDigitos :: Int -> Int
-- sumaDigitos n
--     | div n 10 == 0 = suma
--     |
--     where suma = n

-- correrComaMVeces :: Int -> Int
-- correrComaMVeces n =

-- cantidadDigitos :: Int -> Int
-- cantidadDigitos n

-- ejecutarMVeces :: Int -> Int
-- ejecutarMVeces M funcionAEjecutar
--     | M == 1 = funcionAEjecutar
--     | otherwise = ejecutarMVeces(M-1)

-- sumaDigitos :: Int -> Int
-- sumaDigitos n = ultimoDigito(n) + ultimoDigito(nComaCorrida(n)) + ultimoDigito(nComaCorrida(nComaCorrida(n)))

esDivisible :: Float -> Float -> Bool
esDivisible dd d = dd - cociente*d == 0
    where cociente = parteEntera(dd/d)

{-
sumaImpares :: Int -> Int
sumaImpares n
    | n == 1 = 1
    | mod n 2 == 0 = n + sumaImpares(n-1)
    | otherwise = sumaImpares(n-1)
-}

{-
medioFact :: Int -> Int
medioFact n
    | mod n 2 == 0 = 1
    | otherwise = n * medioFact(n-2)
-}
