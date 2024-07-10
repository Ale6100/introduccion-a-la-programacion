import typing
from queue import LifoQueue as Pila
from queue import Queue as Cola
import random

#? Funciones auxiliares generales
def quitar_numero_de_la_lista(num: int, lista: list[int]):
    i = 0
    while i < len(lista):
        if lista[i] == num:
            lista.pop(i)
        i += 1

#? ETAPA 1: ARCHIVOS
#! Ejercicio 1
#? 1
def contar_lineas(nombre_archivo: str) -> int:
    archivo: typing.IO = open(nombre_archivo, 'r')

    contenido = archivo.readlines()

    archivo.close()
    return len(contenido)

#? 2
def existe_palabra(palabra: str, nombre_archivo: str) -> bool:
    archivo = open(nombre_archivo, 'r')

    contenido = archivo.read()

    archivo.close()
    return palabra in contenido

#? 3
def cantidad_apariciones(nombre_archivo: str, palabra: str) -> int:
    archivo = open(nombre_archivo, 'r')

    contenido = archivo.read()

    contador = 0
    while palabra in contenido:
        contador = contador + 1

        indice_inicia_palabra = contenido.index(palabra)
        contenido = contenido[:indice_inicia_palabra] + contenido[indice_inicia_palabra+len(palabra):]

    archivo.close()
    return contador

#! Ejercicio 2
def clonar_sin_comentarios(nombre_archivo: str):
    archivo = open(nombre_archivo, 'r')
    nuevo_archivo = open('nuevo_archivo', 'w')

    lineas = archivo.readlines()

    for i in range(len(lineas)):
        linea = lineas[i]
        while linea[0] == " ":
            linea = linea[1:]

        if linea[0] != "#":
            nuevo_archivo.writelines(linea)

    archivo.close()

#! Ejercicio 3
def invertir_lineas(nombre_archivo: str):
    archivo = open(nombre_archivo, 'r')
    nuevo_archivo = open('reverso.txt', 'w')

    lineas = archivo.readlines()

    for i in range(len(lineas)-1, -1, -1):
        nuevo_archivo.writelines(lineas[i])

    archivo.close()

#! Ejercicio 4
def agregar_frase_al_final(nombre_archivo: str, frase: str):
    archivo = open(nombre_archivo, 'a')

    archivo.write(frase)
    archivo.close()

#! Ejercicio 5
def agregar_frase_al_principio(nombre_archivo: str, frase: str):
    archivo_original = open(nombre_archivo, 'r')
    contenido_original = archivo_original.read()

    archivo_nuevo = open(nombre_archivo, 'w')
    archivo_nuevo.write(frase + contenido_original)

    archivo_original.close()
    archivo_nuevo.close()

#! Ejercicio 6
def listar_palabras_de_archivo(nombre_archivo: str) -> list[str]:
    archivo = open(nombre_archivo, 'rb')

    contenido = archivo.read()

    lista: list[str] = []
    palabra: str = ""

    for i in range(len(contenido)):
        byte = contenido[i]
        caracter = chr(byte)

        if caracter in ' \n':
            if len(palabra) >= 5:
                lista.append(palabra)
            palabra = ""
        elif caracter in '0123456789abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ_':
            palabra = palabra + caracter

    archivo.close()
    return lista

#! Ejercicio 7
def desestructurar_datos(estudiante_actual: str) -> list[str]:
    datos_desestructurados: list[str] = []
    string_auxiliar = ""
    for caracter in estudiante_actual:
        if caracter != ",":
            string_auxiliar += caracter
        else:
            datos_desestructurados.append(string_auxiliar)
            string_auxiliar = ""
    datos_desestructurados.append(string_auxiliar[:-1])
    return datos_desestructurados

def calcular_promedio_por_estudiante(nombre_archivo_notas: str, nombre_archivo_promedios: str):
    archivo = open(nombre_archivo_notas, 'r', encoding='utf-8')

    contenido = archivo.readlines()

    datos_estudiantes = {}

    for i in range(1, len(contenido)):
        estudiante_actual = contenido[i]

        datos_desestructurados = desestructurar_datos(estudiante_actual)

        LU = datos_desestructurados[0]
        nota = float(datos_desestructurados[-1])

        cantidad_de_notas_actuales, nota_actual = 0, 0
        if LU in datos_estudiantes.keys():
            cantidad_de_notas_actuales = datos_estudiantes[LU]["cantidad_de_notas"]
            nota_actual = datos_estudiantes[LU]["nota"]

        datos_estudiantes[LU] = {
            "nota": nota_actual + nota,
            "cantidad_de_notas": cantidad_de_notas_actuales + 1
        }

    archivo_a_guardar = open(f'{nombre_archivo_promedios}.csv', 'w', encoding='utf-8')
    archivo_a_guardar.writelines("nro de LU,promedio\n")

    for lu, info in datos_estudiantes.items():
        archivo_a_guardar.writelines(f'{lu},{info["nota"]/info["cantidad_de_notas"]}')
        archivo_a_guardar.writelines('\n')

    archivo.close()
    archivo_a_guardar.close()

#? ETAPA 2: PILAS
#! Ejercicio 8
def generar_nros_al_azar(cantidad: int, desde: int, hasta: int) -> Pila[int]:
    p: Pila[int] = Pila()

    for _ in range(cantidad):
        numero_random = random.randint(desde, hasta)
        p.put(numero_random)
    return p

#! Ejercicio 9
def cantidad_elementos(p: Pila) -> int:
    p_aux: Pila = Pila()

    contador = 0
    while not p.empty():
        p_aux.put(p.get())
        contador += 1

    while not p_aux.empty():
        p.put(p_aux.get())

    return contador

#! Ejercicio 10
def buscar_el_maximo(p: Pila[int]) -> int:
    maximo = 0
    p_aux: Pila[int] = Pila()

    while not p.empty():
        elemento = p.get()
        if elemento > maximo:
            maximo = elemento
        p_aux.put(elemento)

    while not p_aux.empty():
        p.put(p_aux.get())

    return maximo

#! Ejercicio 11
def esta_bien_balanceada(s: str) -> bool:
    pila_de_parentesis: Pila[str] = Pila()
    i = 0
    bien_balanceada = True
    while bien_balanceada and i < len(s):
        caracter = s[i]
        if caracter in '()':
            if pila_de_parentesis.empty():
                if caracter == ')':
                    bien_balanceada = False
                else:
                    pila_de_parentesis.put(caracter)
            else:
                ultimo_elemento_parentesis = pila_de_parentesis.get()
                if not (ultimo_elemento_parentesis == '(' and caracter == ')'):
                    pila_de_parentesis.put(ultimo_elemento_parentesis)
                    pila_de_parentesis.put(caracter)
        i += 1
    if not pila_de_parentesis.empty():
        bien_balanceada = False
    return bien_balanceada

#! Ejercicio 12
def evaluar_expresion(s: str) -> float:
    tokens: Pila[str] = Pila()
    for caracter in s:
        if caracter != " ":
            tokens.put(caracter)

    tokens_inversos: Pila[str] = Pila()

    while not tokens.empty():
        tokens_inversos.put(tokens.get())

    while cantidad_elementos(tokens_inversos) >= 3:
        ultimo_token = float(tokens_inversos.get())
        ante_ultimo_token = float(tokens_inversos.get())
        ante_penultimo_token = tokens_inversos.get()
        if ante_penultimo_token == '+':
            tokens_inversos.put(ultimo_token + ante_ultimo_token)
        elif ante_penultimo_token == '-':
            tokens_inversos.put(ultimo_token - ante_ultimo_token)
        elif ante_penultimo_token == '*':
            tokens_inversos.put(ultimo_token * ante_ultimo_token)
        elif ante_penultimo_token == '/':
            tokens_inversos.put(ultimo_token / ante_ultimo_token)
    return tokens_inversos.get()

#? ETAPA 3: COLAS
#! Ejercicio 13
def generar_nros_al_azar_cola(cantidad: int, desde: int, hasta: int) -> Pila[int]:
    p: Pila[int] = generar_nros_al_azar(cantidad, desde, hasta)
    c: Cola[int] = Cola()

    while not p.empty():
        c.put(p.get())

    return c

#! Ejercicio 14
def cantidad_elementos_cola(c: Cola) -> int:
    c_aux: Cola = Cola()

    contador = 0
    while not c.empty():
        c_aux.put(c.get())
        contador += 1

    while not c_aux.empty():
        c.put(c_aux.get())

    return contador

#! Ejercicio 15
def buscar_el_maximo_cola(c: Cola[int]) -> int:
    maximo = 0
    c_aux: Cola[int] = Cola()

    while not c.empty():
        elemento = c.get()
        if elemento > maximo:
            maximo = elemento
        c_aux.put(elemento)

    while not c_aux.empty():
        c.put(c_aux.get())

    return maximo

#! Ejercicio 16
def armar_secuencia_de_bingo() -> Cola[int]:
    lista: list[int] = []
    while len(lista) != 100:
        numero_random = random.randint(0, 99)
        while numero_random in lista:
            numero_random = random.randint(0, 99)
        lista.append(numero_random)

    secuencia: Cola[int] = Cola()
    for num in lista:
        secuencia.put(num)
    return secuencia

def jugar_carton_de_bingo(carton: list[int], bolillero: Cola[int]) -> int:
    cantidad_de_jugadas = 0
    bolillero_aux: Cola[int] = Cola()

    while len(carton) != 0:
        numero_bolillero = bolillero.get()
        bolillero_aux.put(numero_bolillero)
        if numero_bolillero in carton:
            quitar_numero_de_la_lista(numero_bolillero, carton)
        cantidad_de_jugadas += 1

    while not bolillero.empty():
        bolillero_aux.put(bolillero.get())

    while not bolillero_aux.empty():
        bolillero.put(bolillero_aux.get())

    return cantidad_de_jugadas

#! Ejercicio 17
def n_pacientes_urgentes(c: Cola[(int, str, str)]) -> int:
    c_aux: Cola[(int, str, str)] = Cola()
    contador = 0

    while not c.empty():
        paciente = c.get()
        c_aux.put(paciente)

        if paciente[0] <= 3:
            contador += 1

    while not c_aux.empty():
        c.put(c_aux.get())

    return contador

#! Ejercicio 18
def atencion_a_clientes(c: Cola[(str, int, bool, bool)]) -> Cola[(str, int, bool, bool)]:
    cola_final: Cola[(str, int, bool, bool)] = Cola()
    cola_copia : Cola[(str, int, bool, bool)] = Cola()

    c_aux: Cola[(str, int, bool, bool)] = Cola()
    c_aux2: Cola[(str, int, bool, bool)] = Cola()

    while not c.empty():
        persona = c.get()
        cola_copia.put(persona)

        if persona[3] == True:
            cola_final.put(persona)
        else:
            c_aux.put(persona)

    while not c_aux.empty():
        persona = c_aux.get()

        if persona[2] == True:
            cola_final.put(persona)
        else:
            c_aux2.put(persona)

    while not c_aux2.empty():
        cola_final.put(c_aux2.get())

    while not cola_copia.empty():
        c.put(cola_copia.get())

    return cola_final

#? ETAPA 4: DICCIONARIOS
#! Ejercicio 19
def agrupar_por_longitud(nombre_archivo: str) -> dict:
    archivo = open(nombre_archivo, 'r', encoding='utf-8')
    contenido = archivo.read()

    diccionario = {}

    palabra = ""
    for caracter in contenido:
        if caracter == " " or caracter == "\n":
            longitud_palabra = len(palabra)
            if longitud_palabra in diccionario.keys():
                diccionario[longitud_palabra] += 1
            else:
                diccionario[longitud_palabra] = 1
            palabra = ""
        else:
            palabra += caracter

    archivo.close()
    return diccionario

#! Ejercicio 20
# El ejercicio 7 calcular_promedio_por_estudiante es prácticamente igual

#! Ejercicio 21
def la_palabra_mas_frecuente(nombre_archivo: str) -> str:
    archivo: typing.IO = open(nombre_archivo, 'r', encoding='utf-8')

    contenido = archivo.read()

    lista_palabras: list[str] = []
    palabra_aux = ""

    for token in contenido:
        if token != ' ' and token != '\n':
            palabra_aux += token
        else:
            lista_palabras.append(palabra_aux)
            palabra_aux = ''

    diccionario = {}

    for palabra in lista_palabras:
        if palabra in diccionario.keys():
            diccionario[palabra] += 1
        else:
            diccionario[palabra] = 1

    res = ""
    contador = 0
    for palabra, veces_que_aparece in diccionario.items():
        if veces_que_aparece >= contador:
            res = palabra
            contador = veces_que_aparece
    return res

#! Ejercicio 22
#2 Requiere que ya esté registrado
def visitar_sitio(historiales: dict[str, Pila[str]], usuario: str, sitio: str):
    historiales[usuario].put(sitio)

#3 Requiere que ya esté registrado y tenga dos elementos en el historial
def navegar_atras(historiales: dict[str, Pila[str]], usuario: str):
    ultimo = historiales[usuario].get()
    ante_ultimo = historiales[usuario].get()
    historiales[usuario].put(ante_ultimo)
    historiales[usuario].put(ultimo)
    historiales[usuario].put(ante_ultimo)

#! Ejercicio 23
# Requiere que el producto no esté previamente en el diccionario
def agregar_producto(inventario: dict[str, float, int], nombre: str, precio: float, cantidad: int):
    inventario[nombre] = {
        'precio': precio,
        'cantidad': cantidad
    }

# Las dos siguientes funciones requieren que el producto esté previamente en el diccionario
def actualizar_stock(inventario: dict[str, float, int], nombre: str, cantidad: int):
    inventario[nombre]['cantidad'] = cantidad

def actualizar_precios(inventario: dict[str, float, int], nombre: str, precio: float):
    inventario[nombre]['precio'] = precio

def calcular_valor_inventario(inventario: dict[str, float, int]) -> float:
    valor = 0
    for _, info in inventario.items():
        valor += info['precio']*info['cantidad']

    return valor
