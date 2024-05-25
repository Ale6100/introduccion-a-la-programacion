import math

#! Ejercicio 1
def imprimir_hola_mundo():
  print('¡Hola mundo!')

def imprimir_un_verso():
  print('Un verso\nOtro verso')

def raizDe2() -> float:
  return round(math.sqrt(2), 4)

def factorial_de_dos() -> int:
  return 2

def perimetro() -> float:
  return 2*math.pi

#! Ejercicio 2

def imprimir_saludo(nombre: str):
  print(f'Hola {nombre}')

def raiz_cuadrada_de(numero: float) -> float:
  return math.sqrt(numero)

def fahrenheit_a_celsius(t: float) -> float:
  return (t-32)*5/9

def imprimir_dos_veces(estribillo: str):
  print(estribillo*2)

# m debe ser distinto de cero
def es_multiplo_de(n: int, m: int) -> bool:
  return n % m == 0

def es_par(numero: int) -> bool:
  return es_multiplo_de(numero, 2)

def cantidad_de_pizzas(comensales: int, min_cant_de_porciones: int) -> int:
  return math.ceil(comensales*min_cant_de_porciones/8)

#! Ejercicio 3

def alguno_es_0(numero1: int, numero2: int) -> bool:
  return numero1 == 0 or numero2 == 0

def ambos_son_0(numero1: int, numero2: int) -> bool:
  return numero1 == 0 and numero2 == 0

def es_nombre_largo(nombre: str) -> bool:
  return 3 <= len(nombre) <= 8

def es_bisiesto(año: int) -> bool:
  return es_multiplo_de(año, 400) or (es_multiplo_de(año, 4) and not es_multiplo_de(año, 100))

#! Ejercicio 4

# Altura en centímetros
def peso_pino(altura: float):
  peso = 0
  centimetros_sobre_los_3_metros = altura - 300

  if (centimetros_sobre_los_3_metros >= 0):
    peso = 3*300 + centimetros_sobre_los_3_metros*2
  else:
    peso = 3*altura
  return peso

# Peso en kg
def es_peso_util(peso: float) -> bool:
  return 400 <= peso <= 1000

# Altura en centímetros
def sirve_pino(altura: float) -> bool:
  return es_peso_util(peso_pino(altura))

#! Ejercicio 5

def devolver_el_doble_si_es_par(numero: float):
  if (es_par(numero)):
    numero = 2*numero
  return numero

def devolver_valor_si_es_par_sino_el_que_sigue(numero: int):
  if (not es_par(numero)):
    numero = numero + 1
  return numero

def lindo_nombre(nombre: str) -> str:
  return 'Tu nombre tiene muchas letras!' if len(nombre) >= 5 else 'Tu nombre tiene menos de 5 caracteres'

def elRango(numero: float) -> str:
  res = ''
  if numero < 5:
    res = 'Menor a 5'
  elif 10 < numero < 20:
    res = 'Entre 10 y 20'
  elif numero > 20:
    res = 'Mayor a 20'
  return res

def estado_persona(sexo: str, edad: int) -> str:
  texto_anda = 'Andá de vacaciones'
  res = ''
  if edad < 18:
    res = texto_anda
  elif edad >= 60 and sexo == 'F':
    res = texto_anda
  elif edad >= 65 and sexo == 'M':
    res = texto_anda
  else:
    res = 'Te toca trabajar'
  return res

#! Ejercicio 6
def imprimir_del_1_al_10():
  i = 1
  while i <= 10:
    print(i)
    i = i + 1

def imprimir_pares_del_10_al_40():
  i = 10
  while i <= 40:
    print(i)
    i = i + 2

def imprimir_eco():
  i = 1
  while i <= 10:
    print('eco')
    i = i + 1

def cuenta_regresiva(t: int):
  while t != 0:
    print(t)
    t = t - 1
  print('Despegue')

# anio_llegada debe ser más chico que anio_partida
def monitorear_viaje_en_el_tiempo(anio_partida: int, anio_llegada: int):
  anio_actual = anio_partida - 1
  while anio_actual >= anio_llegada:
    print(f'Viajó un año al pasado, estamos en el año {anio_actual}')
    anio_actual = anio_actual - 1

# anio_partida debe ser mayor a -384
def monitorear_hasta_ver_a_aristoteles(anio_partida: int):
  anio_actual = anio_partida - 20
  while anio_actual >= -384:
    print(f'Viajó 20 años al pasado, estamos en el año {anio_actual}')
    anio_actual = anio_actual - 20

#! Ejercicio 7
def imprimir_del_1_al_10_for():
  for i in range(1, 11):
    print(i)

def imprimir_pares_del_10_al_40_for():
  for i in range(10, 41, 2):
    print(i)

def imprimir_eco_for():
  for _ in range(1, 11):
    print('eco')

def cuenta_regresiva_for(t: int):
  for i in range(t, 0, -1):
    print(i)
  print('Despegue')

# anio_llegada debe ser más chico que anio_partida
def monitorear_viaje_en_el_tiempo_for(anio_partida: int, anio_llegada: int):
  for i in range(anio_partida-1, anio_llegada-1, -1):
    print(f'Viajó un año al pasado, estamos en el año {i}')

# anio_partida debe ser mayor a -384
def monitorear_hasta_ver_a_aristoteles_for(anio_partida: int):
  for i in range(anio_partida, -385, -20):
    print(f'Viajó 20 años al pasado, estamos en el año {i}')

#! Ejercicio 8
# 1
x = 5
y = 7

x = x + y
print('x = 12:', x)

#2
x = 5
y = 7
z = x + y
y = z * 2
print('y = 24:', y)

#3
x = 5
y = 7
x = 'hora'
y = x * 2
print('y = horahora:', y)

#4
x = False
res = not(x)
print('res = True:', res)

#5
x = False
x = not(x)
print('x = True:', x)

#6
x = True
y = False
res = x and y
x = res and x
print('x = False:', x)

#! Ejercicio 9
def rt(x: int, g: int) -> int:
  g = g + 1
  return x + g

g: int = 0
def ro(x: int) -> int:
  global g
  g = g + 1
  return x + g

# print('9.1:', ro(1), ro(1), ro(1)) # Retorna 2 3 4
# print('9.2: ', rt(1, 0), rt(1, 0), rt(1, 0)) # Retorna 2 2 2
