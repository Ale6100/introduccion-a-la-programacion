import math

#? PRIMERA PARTE
#! Ejercicio 1
#1
def pertenece(s: list[int], e: int) -> bool:
    for i in range(0, len(s)):
        if s[i] == e:
            return True
    return False

#? 2
# Requiere que e sea distinto de cero
def divide_a_todos(s: list[int], e: int) -> bool:
    for i in range(0, len(s)):
        if s[i] % e != 0:
            return False
    return True

#? 3
def suma_total(s: list[int]) -> int:
    suma = 0
    for i in range(0, len(s)):
        suma = suma + s[i]
    return suma

#? 4
def ordenados(s: list[int]) -> bool:
    for i in range(0, len(s)-1):
        if s[i] >= s[i+1]:
            return False
    return True

#? 5
def alguna_palabra_tiene_longitud_mayor_a_7(lista: list[str]) -> bool:
    for i in range(0, len(lista)):
        if len(lista[i]) > 7:
            return True
    return False

#? 6
def palindromo(palabra: str) -> bool:
    i_maximo = math.ceil(len(palabra)/2) # La mitad si es par, la mitad entera + 1 si es impar
    for i in range(0, i_maximo):
        if palabra[i] != palabra[-i-1]:
            return False
    return True

#? 7
def tiene_minuscula(palabra: str) -> bool:
    abecedario = "abcdefghijklmnñopqrstuvwxyz"
    for letra in palabra:
        if letra in abecedario:
            return True
    return False

def tiene_mayuscula(palabra: str) -> bool:
    abecedario = "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ"
    for letra in palabra:
        if letra in abecedario:
            return True
    return False

def tiene_al_menos_un_digito_numerico(palabra: str) -> bool:
    numeros = "0123456789"
    for i in palabra:
        if i in numeros:
            return True
    return False

def fortaleza(password: str) -> str:
    color = 'AMARILLA'
    length_password = len(password)
    if length_password > 8 and tiene_minuscula(password) and tiene_mayuscula(password) and tiene_al_menos_un_digito_numerico(password):
        color = 'VERDE'
    elif length_password < 5:
        color = 'ROJA'
    return color

#? 8
def saldo_actual(lista: list[tuple[str, int]]) -> int:
    saldo = 0
    for i in range(0, len(lista)):
        numero = lista[i][1]
        if lista[i][0] == "I":
            saldo = saldo + numero
        else:
            saldo = saldo - numero
    return saldo

#? 9
def tiene_vocales(palabra: str) -> bool:
    for letra in palabra:
        if letra in "aeiouAEIOU":
            return True
    return False

def tiene_al_menos_3_vocales_distintas(palabra: str) -> bool:
    cantidad_de_vocales = 0
    vocales_ya_contadas = ""

    while tiene_vocales(palabra):
        letra_actual = palabra[0]
        if letra_actual not in vocales_ya_contadas and tiene_vocales(letra_actual):
            cantidad_de_vocales = cantidad_de_vocales + 1
            vocales_ya_contadas = vocales_ya_contadas + letra_actual
        palabra = palabra[1:]
    return cantidad_de_vocales >= 3

#? Segunda Parte
#! Ejercicio 2
#? 1
def poner_0_en_posiciones_pares(lista: list[int]) -> list[int]:
    for i in range(1, len(lista), 2):
        lista[i] = 0
    return lista

#? 2
def poner_0_en_posiciones_pares_sin_modificar_la_original(lista: list[int]) -> list[int]:
    lista2 = lista.copy()
    for i in range(1, len(lista2), 2):
        lista2[i] = 0
    return lista2

#? 3
def eliminar_vocales(string: str) -> str:
    nuevo_string = ""
    for i in string:
        if i not in "aeiouAEIOU":
            nuevo_string = nuevo_string + i
    return nuevo_string

#? 4
# s es una lista de chars
def reemplaza_vocales(s: list[str]) -> list[str]:
    nuevo_string = []
    for letra in s:
        if letra in "aeiou":
            nuevo_string.append("_")
        else:
            nuevo_string.append(letra)
    return nuevo_string

#? 5
def da_vuelta_str(s: str) -> str:
    nuevo_string = ""
    for i in s:
        nuevo_string = i + nuevo_string
    return nuevo_string

#? 6
# s es una lista de chars
def eliminar_repetidos(s: list[str]) -> list[str]:
    res = []
    for i in s:
        if i not in res:
            res.append(i)
    return res

#! Ejercicio 3
def promedio(lista_numeros: list[int]) -> float:
    return sum(lista_numeros)/len(lista_numeros)

def todos_los_numeros_mayores_o_iguales_a_4(lista_numeros: list[int]) -> bool:
    for num in lista_numeros:
        if num < 4:
            return False
    return True

# notas debe ser una lista no vacía, con números del 0 al 10
def aprobado(notas: list[int]) -> int:
    res = 3
    promedio_ = promedio(notas)
    if todos_los_numeros_mayores_o_iguales_a_4(notas):
        if promedio_ >= 7:
            res = 1
        elif 4 <= promedio_ < 7:
            res = 2
    return res
