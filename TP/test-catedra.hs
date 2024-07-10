import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]

-- Variables a usar en varios testeos
computacion = "computacion"
computacionCifrado3 = "frpsxwdflrq"

abc = "abc"
abcCifrado3 = "def"

ccc = "ccc"
cccCifrado5 = "hhh"

zyx = "zyx"
zyxCifrado5 = "edc"

-- ! Ejercicio 1
-- Testeamos con varias minúsculas, mayúsculas, y con caracteres con tilde
testsEjesMinuscula = test [
    esMinuscula 'd' ~?= True,
    esMinuscula 'A' ~?= False,
    esMinuscula 'z' ~?= True,
    esMinuscula 'D' ~?= False,
    esMinuscula 'Z' ~?= False,
    esMinuscula 'ñ' ~?= False,
    esMinuscula 'á' ~?= False,
    esMinuscula 'Á' ~?= False
    ]

-- ! Ejercicio 2
-- Testeamos con varias letras al principio, final y centro del abecedario
testsEjletraANatural = test [
    letraANatural 'b' ~?= 1,
    letraANatural 'a' ~?= 0,
    letraANatural 'z' ~?= 25,
    letraANatural 'f' ~?= 5,
    letraANatural 'o' ~?= 14
    ]

-- ! Ejercicio 3
-- Probamos con desplazamientos entre 0 a 25 y más grandes, aprovechando la periodicidad. También se testean desplazamientos negativos
testsEjdesplazar = test [
    desplazar 'a' 3 ~?= 'd',
    desplazar 'b' 0 ~?= 'b',
    desplazar 'z' 1 ~?= 'a',
    desplazar 'a' 0 ~?= 'a',
    desplazar 'z' 0 ~?= 'z',
    desplazar 'a' 26 ~?= 'a',
    desplazar 'z' 26 ~?= 'z',
    desplazar 'a' (26*50) ~?= 'a',
    desplazar 'a' (26+5) ~?= 'f',
    desplazar 'f' (26+5) ~?= 'k',
    desplazar 'a' (-1) ~?= 'z',
    desplazar 'a' (-26) ~?= 'a',
    desplazar 'g' (- (26 * 42)) ~?= 'g'
    ]

-- ! Ejercicio 4
-- Probamos con cifrados sin desplazamiento, palabras vacías, y otros múltiples ejemplos que sabemos que deben ser verdaderos
testsEjcifrar = test [
    cifrar computacion 3 ~?= computacionCifrado3,
    cifrar "hola" 0 ~?= "hola",
    cifrar "" 6 ~?= "",
    cifrar "asd" 0 ~?= "asd",
    cifrar "batata" 26 ~?= "batata",
    cifrar "roberto" (26*50) ~?= "roberto",
    cifrar abc 3 ~?= abcCifrado3,
    cifrar zyx 5 ~?= zyxCifrado5,
    cifrar ccc 5 ~?= cccCifrado5,
    cifrar "a" 5 ~?= "f"
    ]

-- ! Ejercicio 5
-- Mismo razonamiento que en el grupo de test del ejercicio 4, pero al revés
testsEjdescifrar = test [
    descifrar computacionCifrado3 3 ~?= computacion,
    descifrar "hola" 0 ~?= "hola",
    descifrar "" 6 ~?= "",
    descifrar abcCifrado3 3 ~?= abc,
    descifrar zyxCifrado5 5 ~?= zyx,
    descifrar cccCifrado5 5 ~?= ccc
    ]

-- ! Ejercicio 6
-- Testeamos si cifra bien las palabras con respecto a su ubicación en la lista y con distintas longitudes, incluso vacía
testsEjcifrarLista = test [
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    cifrarLista ["papa", "perro", "estacion"] ~?= ["papa", "qfssp", "guvcekqp"],
    cifrarLista [] ~?= [],
    cifrarLista [abc, "mundo", "z", abc] ~?= [abc, "nvoep", "b", abcCifrado3]
    ]

-- ! Ejercicio 7
-- Frecuencias, de las palabras "taller", "a", "ab", "aaaaaaa", "adios", "abcde", "wxyz" y "a b c"
-- Se consideraron ejemplos donde a ojo uno sabe identificar que la respuesta es correcta
listaFrecuenciaTaller = [16.666668, 0, 0, 0, 16.666668, 0, 0, 0, 0, 0, 0, 33.333336, 0, 0, 0, 0, 0, 16.666668, 0, 16.666668, 0, 0, 0, 0, 0, 0]
listaFrecuenciaA = [100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
listaFrecuenciaAb = [50, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
listaFrecuenciaAaaaa = [100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
listaFrecuenciaAdios = [20, 0, 0, 20, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0]
listaFrecuenciaNula = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
listaFrecuenciaAbcde = [20, 20, 20, 20, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
listaFrecuenciaxyz = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 25, 25, 25]
listaFrecuenciaAbc = [33.333332, 33.333332, 33.333332, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") listaFrecuenciaTaller,
    expectlistProximity (frecuencia "a") listaFrecuenciaA,
    expectlistProximity (frecuencia "ab") listaFrecuenciaAb,
    expectlistProximity (frecuencia "aaaaaaa") listaFrecuenciaAaaaa,
    expectlistProximity (frecuencia "adios") listaFrecuenciaAdios,
    expectlistProximity (frecuencia "") listaFrecuenciaNula,
    expectlistProximity (frecuencia "abcde") listaFrecuenciaAbcde,
    expectlistProximity (frecuencia "edcba") listaFrecuenciaAbcde,
    expectlistProximity (frecuencia "wxyz") listaFrecuenciaxyz,
    expectlistProximity (frecuencia " a b c " ) listaFrecuenciaAbc,
    expectlistProximity (frecuencia "abc" ) listaFrecuenciaAbc
    ]

-- ! Ejercicio 8
-- No solo se testeó su funcionalidad con desplazamientos positivos, si no también con negativos y cero
resultadoCifradoFrecuentea0 = ('a', 100)

testsEjcifradoMasFrecuente = test [
    expectAnyTuplaAprox (cifradoMasFrecuente "taller" 3) [('o', 33.333336)],
    expectAnyTuplaAprox (cifradoMasFrecuente "a" 2) [('c', 100)],
    expectAnyTuplaAprox (cifradoMasFrecuente "megamente" 3) [('h', 33.333332)],
    expectAnyTuplaAprox (cifradoMasFrecuente "a" 0) [resultadoCifradoFrecuentea0],
    expectAnyTuplaAprox (cifradoMasFrecuente "z" 1) [resultadoCifradoFrecuentea0],
    expectAnyTuplaAprox (cifradoMasFrecuente "a" (26*50)) [resultadoCifradoFrecuentea0],
    expectAnyTuplaAprox (cifradoMasFrecuente "computacion" 3 ) [('f', 18.181818)],
    expectAnyTuplaAprox (cifradoMasFrecuente "aabbcccccc" (-4) ) [('y', 60)],
    expectAnyTuplaAprox (cifradoMasFrecuente "rrrrzt" (-3) ) [('o', 66.666666)]
    ]

-- ! Ejercicio 9
-- Se probó con ejemplos con veracidad fácilmente comprobable, aprovechando las variables ya definidas
testsEjesDescifrado = test [
    esDescifrado "taller" "compu" ~?= False,
    esDescifrado "a" "ab" ~?= False,
    esDescifrado "hola" "adios" ~?= False,
    esDescifrado "a" "a" ~?= True,
    esDescifrado "r" "f" ~?= True,
    esDescifrado "a" "" ~?= False,
    esDescifrado "" "a" ~?= False,
    esDescifrado "" "" ~?= True,
    esDescifrado "perro" "qfssp" ~?= True,
    esDescifrado "abcdef" "defghi" ~?= True,
    esDescifrado computacion computacionCifrado3 ~?= True,
    esDescifrado abc abc ~?= True,
    esDescifrado abc abcCifrado3 ~?= True
    ]

-- ! Ejercicio 10
-- Probamos con listas donde: Todos los strings son descifrados entre ellos, donde ninguno es descifrado de ninguno, y con strings vacios dentro de la lista
combinacionABZ = [("a","b"), ("a", "z"), ("b", "a"), ("b", "z"), ("z", "a"), ("z", "b")]

testsEjtodosLosDescifrados = test [
    todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")],
    todosLosDescifrados ["a", "b", "z"] ~?= combinacionABZ,
    todosLosDescifrados ["abc", "bca", "cdb"] ~?= [("bca", "cdb"), ("cdb", "bca")],
    todosLosDescifrados ["abc", "bcb", "ddb"] ~?= [],
    todosLosDescifrados ["abc", "", "abcd"] ~?= [],
    todosLosDescifrados [] ~?= []
    ]

-- ! Ejercicio 11
-- Probamos extender, recortar y mantener distintas palabras
testsEjexpandirClave = test [
    expandirClave "compu" 8 ~?= "compucom",
    expandirClave "compu" 1 ~?= "c",
    expandirClave "compu" 5 ~?= "compu",
    expandirClave "compu" 4 ~?= "comp",
    expandirClave "computacion" 5 ~?= "compu",
    expandirClave "informatica" 3 ~?= "inf"
    ]

-- ! Ejercicio 12
computacionIp = "kdueciirqdv"

-- En primeros tres casos se testeó la palabra computación con distintos cifrados, luego consigo mismo, y luego invertimos los primeros testes
testsEjcifrarVigenere = test [
    cifrarVigenere computacion "ip" ~?= computacionIp,
    cifrarVigenere computacion "a" ~?= computacion,
    cifrarVigenere computacion "asdef" ~?= "cgptztsfmtn",
    cifrarVigenere computacion computacion ~?= "ecyeomaeqca",
    cifrarVigenere "ip" computacion ~?= "kd",
    cifrarVigenere "a" computacion ~?= "c",
    cifrarVigenere "asdef" computacion ~?= "cgptz"
    ]

-- ! Ejercicio 13
-- Razonamiento similar al ejercicio 12, pero esta vez con descifrado. Se probaron casos opuestos al punto anterior y, tambien se corroboraron los mismos casos obteniendo resultados efectivamente distintos
testsEjdescifrarVigenere = test [
    descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    descifrarVigenere "computacion" "a" ~?= "computacion",
    descifrarVigenere "cpmquuadipn" "ab" ~?= "computacion",
    descifrarVigenere "computacion" "ip" ~?= "uzeamesnazf",
    descifrarVigenere "computacion" "computacion" ~?= "aaaaaaaaaaa",
    descifrarVigenere "a" "computacion" ~?= "y",
    descifrarVigenere "ip" "computacion" ~?= "gb"
    ]

-- ! Ejercicio 14
-- En los primeros dos testeos se comprueba visualmente fácil que aquellas claves que provocan menor desplazamiento son aquellas que están más al principio del abecedario
-- En el tercer testeo se comprueba su funcionamiento con una clave vacía
-- En el cuarto testeo retorna la única clave posible
-- En el quinto test se coloca un caso donde se comprueba que funciona para una clave posicionada al final de la lista
testsEjpeorCifrado = test [
    peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    peorCifrado "programar" ["aaa", "bbb", "ccc"] ~?= "aaa",
    peorCifrado "" ["ip"] ~?= "ip",
    peorCifrado "" ["abc", "abd", "abz"] ~?= "abc",
    peorCifrado "a" ["ip", "b"] ~?= "b",
    peorCifrado "lambda" ["ip", "computacion", "lambda"] ~?= "lambda"
    ]

-- ! Ejercicio 15
combinacionesLambda = [("lambda", "ipipip"), ("lambda", "ip"), ("mbnceb", "ho")]
cifradoLambdaIp = "tpuqlp"
cifradoLambdaB = "mbnceb"

-- En los primeros casos se testea mediante el uso de palabras y claves donde ya sabemos su combinacion previamente
-- En el ultimo caso, se compara lambda, con dos claves que desplazan de la misma forma, para comprobar que devuelva varias tuplas
testsEjcombinacionesVigenere = test [
    combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
    combinacionesVigenere ["computacion"] ["b", "ip"] computacionIp ~?= [("computacion", "ip")],
    combinacionesVigenere ["a", "b", "c"] ["a", "z", "y"] "a" ~?= [("a", "a"), ("b","z"), ("c", "y")],
    combinacionesVigenere [] [] "" ~?= [],
    combinacionesVigenere ["lambda", cifradoLambdaB] ["ho","ipipip", "ip"] cifradoLambdaIp ~?= combinacionesLambda
    ]

-- ? Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat

-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)

-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)
