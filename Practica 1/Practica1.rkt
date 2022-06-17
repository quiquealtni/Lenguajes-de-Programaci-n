#lang plai
;; Ejercicio 1
;; Predicado que recibe un número natural n y devuelve verdadero si n es par, falso en otro caso.
;; esPar? : numner -> boolean
(define (esPar? n) (zero? (modulo n 2)))

;; Ejercicio 2
;; Función que recibe un número natural n y devuelve una lista en orden ascendente,
;; con los números menores o iguales a n.
;; menores : number -> (listof number)
(define (menores n)
  (cond
    [(< n 0) empty]
    [else (for/list ([i (add1 n)]) i)]))

;; Ejercicio 3
;; Función que recibe un número natural n y devuelve una lista en orden ascendente,
;; con los números pares desde 0 hasta n.
;; pares: number -> (listof number)
(define (pares n)
  (cond
    [(< n 0) empty]
    [else (filter esPar? (for/list ([i (add1 n)]) i))]))

;; Ejercicio 4
;; Función que recibe un número n y calcula la suma de los primeros n números naturales al cuadrado.
;; Esta función utiliza a fórmula conocida para esta cuenta.
;; suma-cuadrados: number -> number
(define (suma-cuadrados n)
  (cond
    [(< n 0) (error "Solo se aceptan numeros naturales.")]
    [else (/ (* n (add1 n) (add1 (* 2 n))) 6)]))

;; Ejercicio 5n
;; Función recursiva, que calcula la suma de los primeros n números naturales al cuadrado.
;; Esta función no utiliza la fórmula conocida, ni directa ni indirectamente.
;; suma-cuadrados: number -> number
(define (suma-cuadradosR n)
  (cond
    [(< n 0) (error "Solo se aceptan numeros naturales.")]
    [(zero? n) 0]
    [else (+ (* n n) (suma-cuadradosR (sub1 n)))]))

;; Ejercicio 6
;; Función que recibe los términos a, b y c,  de una expresión cuadrática y decide si la expresión tiene
;; raíces reales. La función verifica que el discriminante sea mayor o igual a cero.
;; raicesReales? : number number number -> boolean
(define (raicesReales? a b c) (>= (- (* b b) (* 4 a c)) 0))

;; Ejercicio 7
;; Función que recibe tres números a, b y c y devuelve la primer raíz de la fórmula general (sumando la raíz cuadrada)
;; general1: number number number -> number
(define (general1 a b c)
  (cond
    [(not (raicesReales? a b c)) (error "El polinomio no tiene raices reales.")]
    [else (/ (+ (* b -1) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))]))

;; Ejercicio 8
;; Función que recibe tres números a, b y c y devuelve la segunda raíz de la fórmula general (restando la raíz cuadrada)
;; general2: number number number -> number
(define (general2 a b c)
  (cond
    [(not (raicesReales? a b c)) (error "El polinomio no tiene raices reales.")]
    [else (/ (- (* b -1) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))]))

;; Ejercicio 9
;; Función que nos da una lista invertida de la lista pasada como parámetro
;; reversa-lista: (listof a) -> (listof a)
(define (reversa-lista lista)
  (cond
    [(empty? lista) empty]
    [else (append (reversa-lista (cdr lista)) (list (car lista)))]))

;; Ejercicio 10
;; Predicado que nos dice si una lista contiene elementos que forman un palíndromo
;; palindromo-lista?: (listof a) -> Boolean
(define (palindromo? lista) (equal? lista (reversa-lista lista)))