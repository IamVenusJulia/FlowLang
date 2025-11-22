#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLOWLANG
;; Venus Juliana Paipilla y Daniel Arias Castrillón
;; Proyecto Final - Fundamentos de Lenguajes de Programación
;; Profesor: Robinson Duque, Ph.D
;; Universidad del Valle, 2025
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Especificación léxica

(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)
    (identificador
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)))

;Gramática simplificada y corregida

(define especificacion-gramatical
  '((programa (expresion) un-programa)

    ;expresiones básicas
    (expresion (numero) literal-numero)
    (expresion (cadena) literal-cadena)
    (expresion (identificador (arbno "." identificador)) acceso-identificador)
    (expresion ("true") verdadero-exp)
    (expresion ("false") falso-exp)
    (expresion ("null") nulo-exp)
    (expresion ("this") este-exp)


    ;declaraciones y asignaciones
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) 
               declaracion-var)
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion) 
               declaracion-const)
    (expresion ("prototipo" identificador "=" expresion "in" expresion) 
               declaracion-prototipo)
    (expresion ("set" identificador "=" expresion) 
               asignacion-exp)
    
;estructurs de control
    (expresion
      ("if" expresion "then" expresion "else" expresion "end")
      condicional-si)
    (expresion
      ("switch" expresion (arbno "case" expresion ":" expresion) "default" ":" expresion "end")
      condicional-switch)

;Funciones
    (expresion
      ("func" "(" (separated-list identificador ",") ")" expresion)
      definicion-funcion)
    (expresion
      ("(" expresion (arbno expresion) ")")
      aplicacion-funcion)
    (expresion
      ("letrec" (separated-list identificador "(" (separated-list identificador ",") ")" "=" expresion ";")
       "in" expresion)
      letrec-exp)
    
    ;numeros complejos
    (expresion ("complejo" "(" expresion "," expresion ")") 
               complejo-exp)

    ;primitivas e iteraciones
    (expresion
      (primitiva "(" (separated-list expresion ",") ")")
      aplicacion-primitiva)
    
    (expresion
      ("while" expresion "do" expresion "done")
      iteracion-mientras)
    (expresion
      ("for" identificador "in" expresion "do" expresion "done")
      iteracion-para)

    

    ;secuencias
    (expresion
      ("begin" expresion (arbno ";" expresion) "end")
      secuencia-begin)

    ;literales
    (expresion ("[" (separated-list expresion ",") "]") 
               literal-lista)

    ; Primitivas
    (primitiva ("+") primitiva-suma)
    (primitiva ("-") primitiva-resta)
    (primitiva ("*") primitiva-multiplicacion)
    (primitiva ("/") primitiva-division)
    (primitiva ("mod") primitiva-modulo)
    (primitiva ("add1") primitiva-incremento)
    (primitiva ("sub1") primitiva-decremento)
    (primitiva ("zero?") primitiva-cero?)
    (primitiva ("<") primitiva-menor)
    (primitiva (">") primitiva-mayor)
    (primitiva ("<=") primitiva-menor-igual)
    (primitiva (">=") primitiva-mayor-igual)
    (primitiva ("==") primitiva-igual)
    (primitiva ("<>") primitiva-diferente)
    (primitiva ("and") primitiva-y)
    (primitiva ("or") primitiva-o)
    (primitiva ("not") primitiva-no)
    (primitiva ("longitud") primitiva-longitud)
    (primitiva ("concatenar") primitiva-concatenar)
    (primitiva ("vacio") primitiva-lista-vacia)
    (primitiva ("vacio?") primitiva-lista-vacia?)
    (primitiva ("crear-lista") primitiva-construir-lista)
    (primitiva ("lista?") primitiva-es-lista?)
    (primitiva ("cabeza") primitiva-cabeza)
    (primitiva ("cola") primitiva-cola)
    (primitiva ("append") primitiva-concatenar-listas)
    (primitiva ("ref-list") primitiva-referencia-lista)
    (primitiva ("set-list") primitiva-asignar-lista)
    (primitiva ("crear-diccionario") primitiva-crear-diccionario)
    (primitiva ("diccionario?") primitiva-es-diccionario?)
    (primitiva ("ref-diccionario") primitiva-referencia-diccionario)
    (primitiva ("set-diccionario") primitiva-asignar-diccionario)
    (primitiva ("claves") primitiva-obtener-claves)
    (primitiva ("valores") primitiva-obtener-valores)
    (primitiva ("clone") primitiva-clonar)
    (primitiva ("print") primitiva-imprimir)
    (primitiva ("real") primitiva-parte-real)
    (primitiva ("imag") primitiva-parte-imaginaria)
    (primitiva ("get-field") primitiva-obtener-campo)
    (primitiva ("call-method") primitiva-invocar-metodo)
    ))

; ==================== GENERAR TIPOS DE DATOS ====================

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;Parser y Scanner
(define analizar-cadena
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

; ==================== IMPLEMENTACIÓN DEL INTERPRETADOR ====================


; Definición de tipos para valores expresados
(define-datatype valor-expresado valor-expresado?
  (valor-numero (num number?))
  (valor-complejo (parte-real number?) (parte-imag number?))
  (valor-booleano (bool boolean?))
  (valor-cadena (str string?))
  (valor-nulo)
  (valor-lista (elementos vector?))
  (valor-prototipo (campos vector?))
  (valor-procedimiento (proc procedimiento?)))

(define valor-verdadero?
  (lambda (val)
    (cases valor-expresado val
      (valor-booleano (b) b)
      (valor-numero (n) (not (zero? n)))
      (valor-cadena (s) (not (string=? s "")))
      (valor-nulo () #f)
      (else #t))))

;Definición de procedimientos
(define-datatype procedimiento procedimiento?
  (cierre
    (parametros (list-of symbol?))
    (cuerpo expresion?)
    (ambiente-guardado ambiente?)))


;Ambientes
(define-datatype procedimiento procedimiento?
  (cierre
    (parametros (list-of symbol?))
    (cuerpo expresion?)
    (ambiente-guardado ambiente?)))

; Ambiente mejorado con mutabilidad
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record 
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

; Funciones auxiliares para ambientes mutables
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vec env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (vector-ref vec pos)
              (apply-env env sym)))))))

(define set-env!
  (lambda (env sym val)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'set-env! "No binding for ~s" sym))
      (extended-env-record (syms vec env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (vector-set! vec pos val)
              (set-env! env sym val)))))))

; Funciones auxiliares para listas
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else 
        (let ((list-index-r (list-index pred (cdr ls))))
          (if (number? list-index-r)
              (+ list-index-r 1)
              #f))))))

; Función para modificar listas (versión funcional sin mutación)
(define list-set
  (lambda (lst index val)
    (cond
      ((null? lst) 
       (eopl:error 'list-set "Índice fuera de rango"))
      ((zero? index) 
       (cons val (cdr lst)))
      (else 
       (cons (car lst) (list-set (cdr lst) (- index 1) val))))))

; Función auxiliar para verificar si todos los elementos cumplen un predicado
(define all-pairs?
  (lambda (lst)
    (cond
      ((null? lst) #t)
      ((pair? (car lst)) (all-pairs? (cdr lst)))
      (else #f))))

; Función auxiliar para remover elementos de listas (para diccionarios)
(define remove-assoc
  (lambda (key dict)
    (cond
      ((null? dict) '())
      ((equal? (caar dict) key) (remove-assoc key (cdr dict)))
      (else (cons (car dict) (remove-assoc key (cdr dict)))))))

; Evaluar múltiples expresiones
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

; Evaluador principal
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
        (eval-expression exp (init-env))))))

; Ambiente inicial mejorado
(define init-env
  (lambda ()
    (extend-env
     '(x y z pi)
     '(4 2 5 3.14159)
     (empty-env))))

; Aplicar primitivas expandidas
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ; Aritméticas
      (add-prim () (apply + args))
      (substract-prim () (apply - args))
      (mult-prim () (apply * args))
      (div-prim () (apply / args))
      (mod-prim () (apply modulo args))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      
      ; Comparación
      (zero-test-prim () (zero? (car args)))
      (greater-prim () (> (car args) (cadr args)))
      (less-prim () (< (car args) (cadr args)))
      (greater-equal-prim () (>= (car args) (cadr args)))
      (less-equal-prim () (<= (car args) (cadr args)))
      (equal-prim () (equal? (car args) (cadr args)))
      (not-equal-prim () (not (equal? (car args) (cadr args))))
      
      ; Lógicas
      (and-prim () (and (car args) (cadr args)))
      (or-prim () (or (car args) (cadr args)))
      (not-prim () (not (car args)))
      
      ; Strings
      (strlen-prim () (string-length (car args)))
      (strcat-prim () (string-append (car args) (cadr args)))
      
      ; Listas
      (empty-test-prim () (null? (car args)))
      (cons-prim () (cons (car args) (cadr args)))
      (list-test-prim () (list? (car args)))
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (append-prim () (append (car args) (cadr args)))
      (list-ref-prim () 
        (if (< (cadr args) (length (car args)))
            (list-ref (car args) (cadr args))
            (eopl:error 'list-ref-prim "Índice fuera de rango")))
      (list-set-prim () 
        (let ((lst (car args))
              (index (cadr args))
              (val (caddr args)))
          (if (< index (length lst))
              (list-set lst index val)
              (eopl:error 'list-set-prim "Índice fuera de rango"))))
      
      ; Diccionarios (implementación básica)
      (dict-test-prim () 
        (and (list? (car args)) (all-pairs? (car args))))
      (make-dict-prim () '())
      (dict-ref-prim () 
        (let ((dict (car args))
              (key (cadr args)))
          (let ((pair (assoc key dict)))
            (if pair (cdr pair) 'null))))
      (dict-set-prim () 
        (let ((dict (car args))
              (key (cadr args))
              (val (caddr args)))
          (cons (cons key val) (remove-assoc key dict))))
      (keys-prim () (map car (car args)))
      (values-prim () (map cdr (car args))))))

; true-value?
(define true-value?
  (lambda (x)
    (cond
      ((number? x) (not (zero? x)))
      ((boolean? x) x)
      ((null? x) #f)
      (else #t))))

; Función de evaluación de expresiones principal
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      ; Expresiones básicas
      (lit-exp (datum) datum)
      (str-exp (str) str)
      (var-exp (id) (apply-env env id))
      (true-exp () #t)
      (false-exp () #f)
      (null-exp () 'null)
      (empty-exp () '())
      (paren-exp (exp) (eval-expression exp env))
      
      ; Declaraciones y asignaciones
      (var-decl-exp (id exp)
        (let ((val (eval-expression exp env)))
          (extend-env (list id) (list val) env)
          val))
      (const-decl-exp (id exp)
        (let ((val (eval-expression exp env)))
          (extend-env (list id) (list val) env)
          val))
      (assign-exp (id exp)
        (let ((val (eval-expression exp env)))
          (set-env! env id val)
          val))
      
      ; Estructuras de control
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
            (eval-expression true-exp env)
            (eval-expression false-exp env)))
      (while-exp (test-exp body-exp)
        (let loop ()
          (when (true-value? (eval-expression test-exp env))
            (eval-expression body-exp env)
            (loop)))
        'null)
      (for-exp (var iterable-exp body-exp)
        (let ((items (eval-expression iterable-exp env)))
          (for-each
           (lambda (item)
             (let ((new-env (extend-env (list var) (list item) env)))
               (eval-expression body-exp new-env)))
           items)
          'null))
      (begin-exp (first-exp rest-exps)
        (let ((all-exprs (cons first-exp rest-exps)))
          (if (null? all-exprs)
              (eopl:error 'eval-expression "Begin vacío")
              (let loop ((exprs all-exprs))
                (let ((current-val (eval-expression (car exprs) env)))
                  (if (null? (cdr exprs))
                      current-val
                      (loop (cdr exprs))))))))
      
      ; Funciones
      (func-exp (params body)
        (proc-val (procedure params body env)))
      (return-exp (exp) (eval-expression exp env))
      
      ; Llamadas a funciones
      (func-call-exp (func-id args)
        (let ((proc-value (apply-env env func-id)))
          (cases expval proc-value
            (proc-val (actual-proc)
              (cases proc actual-proc
                (procedure (params body saved-env)
                  (let ((arg-values (eval-rands args env)))
                    (if (= (length params) (length arg-values))
                        (let ((new-env (extend-env params arg-values saved-env)))
                          (eval-expression body new-env))
                        (eopl:error 'func-call-exp 
                                   "Número incorrecto de argumentos. Esperaba ~a, obtuvo ~a"
                                   (length params) (length arg-values)))))))
            (else (eopl:error 'func-call-exp "No es una función: ~s" func-id)))))
      
      ; Listas
      (list-exp (elements)
        (map (lambda (elem) (eval-expression elem env)) elements))
      
      ; Primitivas
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args))))))

; Interpretador (REPL)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser 
      scanner-spec-flowlang
      grammar-flowlang)))

(interpretador)
