module Practica4 where

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
sucesion :: Int -> Float
sucesion 1 = 2
sucesion n = 2 + (1 / sucesion (n-1))

-- n debe ser natural
raizDe2Aprox :: Int -> Float
raizDe2Aprox 1 = 1
raizDe2Aprox n = sucesion n - 1

-- ! Ejercicio 13
sumaPequenia13 :: Int -> Int -> Int
sumaPequenia13 n 1 = n
sumaPequenia13 n m = n^m + sumaPequenia13 n (m-1)

-- n y m deben ser naturales
fnm :: Int -> Int -> Int
fnm 1 m = sumaPequenia13 1 m
fnm n m = sumaPequenia13 n m + fnm (n-1) m

-- ! Ejercicio 14 (sumaPotencias)
-- Es básicamente un copy/paste del anterior
-- q, n y m deben ser naturales
sumaPequenia14 :: Int -> Int -> Int -> Int
sumaPequenia14 q n 1 = q^(n+1)
sumaPequenia14 q n m = q^(n+m) + sumaPequenia14 q n (m-1)

sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q 1 m = sumaPequenia14 q 1 m
sumaPotencias q n m = sumaPequenia14 q n m + sumaPotencias q (n-1) m

-- ! Ejercicio 15 (sumaRacionales)
-- n y m deben ser naturales
sumaPequenia15 :: Int -> Int -> Float
sumaPequenia15 n 1 = fromIntegral n :: Float
sumaPequenia15 n m = (fromIntegral n :: Float)/(fromIntegral m :: Float) + sumaPequenia15 n (m-1)

sumaRacionales :: Int -> Int -> Float
sumaRacionales 1 m = sumaPequenia15 1 m
sumaRacionales n m = sumaPequenia15 n m + sumaRacionales (n-1) m

-- ! Ejercicio 16
-- a)
-- Ejercicio 16 a), práctica 4
-- n = Número natural al cual se le busca el menor divisor
-- i = Número natural que se va incrementando hasta encontrar el menor divisor
menorDivisorAux :: Int -> Int -> Int
menorDivisorAux 1 _ = 1 -- Caso excepcional, cuando n=1 el menor divisor es 1
menorDivisorAux n i
  | mod n i == 0 = i -- Si el i actual es divisor de n, entonces lo retornamos
  | otherwise = menorDivisorAux n (i+1) -- Si no, analizamos el siguiente i hasta encontrar el que buscamos

-- n debe ser natural. Retorna el menor divisor mayor a 1 de n
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorAux n 2 -- Genero una función auxuliar que retorne el menor divisor

-- b)
-- n debe ser natural
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

-- c)
valorMasGrande :: Int -> Int -> Int
valorMasGrande n m
  | n >= m = n
  | otherwise = m

valorMasChico :: Int -> Int -> Int
valorMasChico n m
  | n < m = n
  | otherwise = m

-- Usé el "Método de la resta"
maximoComunDivisor :: Int -> Int -> Int
maximoComunDivisor _ 1 = 1
maximoComunDivisor 1 _ = 1
maximoComunDivisor n m
  | resta == 0 = m
  | otherwise = maximoComunDivisor valorMasGrande_ valorMasChico_
  where
    n_ = valorMasGrande n m
    m_ = valorMasChico n m
    resta = n_ - m_
    valorMasGrande_ = valorMasGrande resta m_
    valorMasChico_ = valorMasChico resta m_

-- n y m deben ser naturales
sonCoprimos :: Int -> Int -> Bool
sonCoprimos n m = maximoComunDivisor n m == 1

-- d)
-- n = n-ésimo primo
-- i = Contador de primos
-- p = Número que inicia en 1 y crece hasta ser primo por n-ésima vez
nEsimoPrimoAux :: Int -> Int -> Int -> Int
nEsimoPrimoAux n i p
  | esPrimo p && (i+1) /= n = nEsimoPrimoAux n (i+1) (p+1)
  | esPrimo p && (i+1) == n = p
  | otherwise = nEsimoPrimoAux n i (p+1)

-- n debe ser natural
nEsimoPrimo :: Int -> Int
nEsimoPrimo n = nEsimoPrimoAux n 0 1

-- ! Ejercicicio 17
esFibonacciAux :: Int -> Int -> Bool
esFibonacciAux n i
  | fibo < n = esFibonacciAux n (i+1)
  | fibo > n = False
  | otherwise = True
  where fibo = fibonacci i

-- n debe ser natural o cero
esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciAux n 0

-- ! Ejercicio 18
esPar :: Int -> Bool
esPar n = mod n 2 == 0

-- n = Número al cual le buscamos el mayor dígito par
-- ma = El mayor dígito par encontrado
-- d = El dígito que estamos analizando actualmente
mayorDigitoParAux :: Int -> Int -> Int -> Int
mayorDigitoParAux n ma d
  | cantidadDeCifras n < d = ma
  | esPar iesimoDigito_ && iesimoDigito_ > ma = mayorDigitoParAux n iesimoDigito_ (d+1)
  | otherwise = mayorDigitoParAux n ma (d+1)
  where
    iesimoDigito_ = iesimoDigito n d

-- n debe ser natural
mayorDigitoPar :: Int -> Int
mayorDigitoPar n = mayorDigitoParAux n (-1) 1

-- ! Ejercicio 19
sumaDeKPrimos :: Int -> Int
sumaDeKPrimos 1 = 2
sumaDeKPrimos k = nEsimoPrimo k + sumaDeKPrimos (k-1)

-- n = Número al cual se quiere saber si es suma "inicial" de primos (n = 1 + 3 + ..., la suma debe inicia en 1)
-- k = k-ésimo primo que se está utilizando como último término de la suma
esSumaInicialDePrimosAux :: Int -> Int -> Bool
esSumaInicialDePrimosAux n k
  | sumaDeKPrimos_ < n = esSumaInicialDePrimosAux n (k+1)
  | sumaDeKPrimos_ == n = True
  | sumaDeKPrimos_ > n = False
  where sumaDeKPrimos_ = sumaDeKPrimos k

-- n debe ser natural o cero
esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosAux n 1

-- ! Ejercicio 20
-- n = Número al cual queremos saber la suma de sus divisores
-- i = Número i-ésimo que se está estudiando
-- s = Suma, crecerá poco a poco a medida que encuentre divisores
sumaDivisoresAux :: Int -> Int -> Int -> Int
sumaDivisoresAux n m s
  | n == m = s + m
  | esDivisible n m = sumaDivisoresAux n (m+1) (s+m)
  | otherwise = sumaDivisoresAux n (m+1) s

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresAux n 1 0

-- n1 = Valor inicial del rango
-- n2 = Valor final del rango
-- ni = Número entre n1 y n2 a analizar
-- mGuardado = Será el m que finalmente se retornará. Cambiará varias veces hasta llegar al correcto
tomaValorMaxAux :: Int -> Int -> Int -> Int -> Int
tomaValorMaxAux n1 n2 ni mGuardado
  | ni >= n2 = mGuardado
  | mGuardado == -1 || sumaDivisores_ > sumaDivisoresMGuardado_ = tomaValorMaxAux n1 n2 (ni+1) ni
  | otherwise = tomaValorMaxAux n1 n2 (ni+1) mGuardado
  where
    sumaDivisores_ = sumaDivisores ni
    sumaDivisoresMGuardado_ = sumaDivisores mGuardado

-- Hay que encontrar un número m entre n1 y n2 tal que ningún otro número en ese rango tenga una suma de divisores mayor que la suma de divisores de m
-- n1 y n2 deben ser naturales, y n2 >= n1. Agregué el hecho de que si n2 = n1 + 1 o son iguales, retorna -1
tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2
  | n1 == n2 || n2 == n1 + 1 = -1
  | n2 - n1 == 1 = n1+1
  | otherwise = tomaValorMaxAux n1 n2 (n1+1) (-1)

-- ! Ejercicio 21
contar :: Int -> Int -> Int -> Int
contar m 0 r
  | m^2 <= r^2 = 1
  | otherwise = 0
contar m n r
  | m^2 + n^2 <= r^2 = 1 + contar m (n-1) r
  | otherwise = contar m (n-1) r

-- m, n y r deben ser números naturales o cero
pitagoras :: Int -> Int -> Int -> Int
pitagoras 0 n r = contar 0 n r
pitagoras m n r = contar m n r + pitagoras (m-1) n r
