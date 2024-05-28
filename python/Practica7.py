import math, random
import numpy as np

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

#! Ejercicio 4
#? 1
def solicitar_nombres() -> list[str]:
    nombres = []
    continuar = True
    while continuar:
        nombre = input('Proporcione un nombre. Escriba "listo" pa finalizar\n')
        if nombre == 'listo':
            continuar = False
        else:
            nombres.append(nombre)
    return nombres

#? 2
def historial_monedero_electronico() -> list[tuple[str, int]]:
    historial = []
    continuar = True
    while continuar:
        input_value = input('''
                            Ingrese la operación:
                            C: Cargar créditos
                            D: Descontar créditos
                            X: Finalizar la simulación
                            ''')
        if input_value == 'C' or input_value == 'D':
            input_monto = input('Ingrese un monto: ')
            historial.append((input_value, input_monto)) # Debería chequear que es un número pero se supone que no sabemos hacer eso todavía
            print('Registrado exitósamente')
        elif input_value == 'X':
            continuar = False
        else:
            print('Operación no válida')
    print('Programa finalizado')
    return historial

#? 3
def juego_7_y_medio():
    historial_cartas = []
    historial_puntos = []
    continuar = True
    while continuar:
        numero = random.randint(0, 12)

        while numero == 8 or numero == 9:
            numero = random.randint(0, 12)

        historial_cartas.append(numero)
        punto = 0.5 if numero in [10, 11, 12] else numero
        historial_puntos.append(punto)

        suma = sum(historial_puntos)

        if suma > 7.5:
            print(f'Has perdido! La suma de tus cartas es {suma}')
            break

        mensaje_tomar_carta = True
        while mensaje_tomar_carta:
            input_value = input(f'''
                                Cartas elegidas: {len(historial_cartas)}
                                Carta tomada. Deseas sacar otra carta del mazo o plantarte?
                                T: Tomar carta
                                P: Plantarse
                                ''')
            if input_value == 'T':
                mensaje_tomar_carta = False
            elif input_value == 'P':
                mensaje_tomar_carta = False
                continuar = False

    print(f'Historial de cartas: \n{historial_cartas} \n\nHistorial de puntos: \n{historial_puntos} \n\nPuntos: \n{sum(historial_puntos)}')

#! Ejercicio 5
#? 1
def pertenece_a_cada_uno_version_1(s: list[list[int]], e: int, res: list[bool]):
    for i in range(0, len(s)):
        res.insert(i, pertenece(s[i], e))

#? 2
def pertenece_a_cada_uno_version_2(s: list[list[int]], e: int, res: list[bool]):
    res.clear()

    for i in range(0, len(s)):
        res.append(pertenece(s[i], e))

#? 3
def es_matriz(s: list[list[int]]) -> bool:
    if len(s) <= 0:
        return False

    longitud_fila_1 = len(s[0])

    for i in range(1, len(s)):
        if len(s[i]) != longitud_fila_1:
            return False
    return True

#? 4
# Requiere que m sea una matriz
def filas_ordenadas(m: list[list[int]], res: list[bool]) -> list[bool]:
    for i in range(0, len(m)):
        res[i] = ordenados(m[i])
    return res

#? 5
def transponer_matriz(m: list[list[int]]):
    m_transpuesto = np.random.random((len(m[0]), len(m)))
    for i in range(0, len(m)):
        for j in range(0, len(m[i])):
            m_transpuesto[j][i] = m[i][j]
    return m_transpuesto

# Requiere que a y b tengan la misma longitud
def producto_vectorial(a: list[int], b: list[int]):
    suma = 0
    for i in range(0, len(a)):
        suma = suma + a[i] * b[i]
    return suma

def multiplicar_matriz_a_por_b(a: list[list[int]], b: list[list[int]]):
    res = np.random.random((len(a), len(b[0]))) # No nos interesa el contenido de esta matriz, luego iré reemplazando sus valores

    b_transpuesto = transponer_matriz(b)

    for i in range(0, len(a)): # Filas de a
        fila = a[i]
        for j in range(0, len(b_transpuesto)):
            res[i][j] = producto_vectorial(fila, b_transpuesto[j])

    return res

def generar_matriz_de_tamanio_d_y_elevarla_a_la_potencia_p(d: int, p: int) -> list[list[int]]:
    matriz_cuadrada_de_tamanio_d = np.random.random((d, d)) # Valores al azar entre 0 y 1
    matriz_copia = matriz_cuadrada_de_tamanio_d.copy()

    for _ in range(p-1):
        matriz_cuadrada_de_tamanio_d = multiplicar_matriz_a_por_b(matriz_cuadrada_de_tamanio_d, matriz_copia)
    return matriz_cuadrada_de_tamanio_d

#! Ejercicio extra
def es_par(n: int) -> bool:
    return n % 2 == 0

def extraer_subsecuencia_de_pares_mas_larga(s: list[int]) -> list[str]:
    subsecuencia_de_pares_mas_larga = []

    i = 0
    while i < len(s):
        d = 0
        subsecuencia_de_pares_temporal = []

        while i+d != len(s) and es_par(s[i+d]):
            subsecuencia_de_pares_temporal.append(s[i+d])
            d = d + 1

        if len(subsecuencia_de_pares_mas_larga) <= len(subsecuencia_de_pares_temporal):
            subsecuencia_de_pares_mas_larga = subsecuencia_de_pares_temporal

        if d == 0:
            i = i + 1
        else:
            i = i + d

    return subsecuencia_de_pares_mas_larga
