#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLOWLANG
;; Venus Juliana Paipilla y Daniel Arias Castrillón
;; Proyecto Final - Fundamentos de Lenguajes de Programación
;; Profesor: Robinson Duque, Ph.D
;; Universidad del Valle, 2025
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Especificación léxica
(define scanner-spec-flowlang
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (string ("\"" (arbno (or (not #\") "\\\"")) "\"") string)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

; Gramática corregida
(define grammar-flowlang
  '((program (expression) a-program)
    
    ; Expresiones atómicas
    (atomic-expression (number) lit-exp)
    (atomic-expression (string) str-exp)
    (atomic-expression (identifier) var-exp)
    (atomic-expression ("true") true-exp)
    (atomic-expression ("false") false-exp)
    (atomic-expression ("null") null-exp)
    
    ; Expresiones compuestas
    (expression (atomic-expression) atomic-exp)
    (expression ("(" expression ")") paren-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("begin" expression (arbno expression) "end") begin-exp)
    
    ; Primitivas
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("zero?") zero-test-prim)
    (primitive (">") greater-prim)
    (primitive ("<") less-prim)
    (primitive ("longitud") strlen-prim)
    (primitive ("concatenar") strcat-prim)))

; ==================== GENERAR TIPOS DE DATOS ====================
(sllgen:make-define-datatypes scanner-spec-flowlang grammar-flowlang)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-flowlang grammar-flowlang)))

; Parser y Scanner
(define scan&parse
  (sllgen:make-string-parser scanner-spec-flowlang grammar-flowlang))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-flowlang grammar-flowlang))

; ==================== IMPLEMENTACIÓN DEL INTERPRETADOR ====================

; Ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record 
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

(define scheme-value? (lambda (v) #t))

; Funciones auxiliares para ambientes
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))

; Funciones auxiliares para listas
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

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

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     '(4 2 5)
     (empty-env))))

; Aplicar primitivas
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () 
        (if (= (length args) 2)
            (+ (car args) (cadr args))
            (eopl:error 'apply-primitive "Primitiva + requiere 2 argumentos")))
      (substract-prim () 
        (if (= (length args) 2)
            (- (car args) (cadr args))
            (eopl:error 'apply-primitive "Primitiva - requiere 2 argumentos")))
      (mult-prim () 
        (if (= (length args) 2)
            (* (car args) (cadr args))
            (eopl:error 'apply-primitive "Primitiva * requiere 2 argumentos")))
      (incr-prim () 
        (if (= (length args) 1)
            (+ (car args) 1)
            (eopl:error 'apply-primitive "Primitiva add1 requiere 1 argumento")))
      (decr-prim () 
        (if (= (length args) 1)
            (- (car args) 1)
            (eopl:error 'apply-primitive "Primitiva sub1 requiere 1 argumento")))
      (zero-test-prim () 
        (if (= (length args) 1)
            (if (zero? (car args)) 1 0)
            (eopl:error 'apply-primitive "Primitiva zero? requiere 1 argumento")))
      (greater-prim () 
        (if (= (length args) 2)
            (if (> (car args) (cadr args)) 1 0)
            (eopl:error 'apply-primitive "Primitiva > requiere 2 argumentos")))
      (less-prim () 
        (if (= (length args) 2)
            (if (< (car args) (cadr args)) 1 0)
            (eopl:error 'apply-primitive "Primitiva < requiere 2 argumentos")))
      (strlen-prim () 
        (if (= (length args) 1)
            (string-length (car args))
            (eopl:error 'apply-primitive "Primitiva longitud requiere 1 argumento")))
      (strcat-prim () 
        (if (= (length args) 2)
            (string-append (car args) (cadr args))
            (eopl:error 'apply-primitive "Primitiva concatenar requiere 2 argumentos"))))))

; true-value?
(define true-value?
  (lambda (x)
    (cond
      ((number? x) (not (zero? x)))
      ((boolean? x) x)
      (else #t))))

; Evaluador de expresiones atómicas
(define eval-atomic-expression
  (lambda (atomic-exp env)
    (cases atomic-expression atomic-exp
      (lit-exp (datum) datum)
      (str-exp (str) str)
      (var-exp (id) (apply-env env id))
      (true-exp () #t)
      (false-exp () #f)
      (null-exp () 'null))))

; Función de evaluación de expresiones principal
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (atomic-exp (atomic) (eval-atomic-expression atomic env))
      (paren-exp (exp) (eval-expression exp env))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (begin-exp (first-exp rest-exps)
        (let ((all-exprs (cons first-exp rest-exps)))
          (if (null? all-exprs)
              (eopl:error 'eval-expression "Begin vacío")
              (let loop ((exprs all-exprs))
                (let ((current-val (eval-expression (car exprs) env)))
                  (if (null? (cdr exprs))
                      current-val
                      (loop (cdr exprs))))))))
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
            (eval-expression true-exp env)
            (eval-expression false-exp env)))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env)))))))

; Interpretador (REPL)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser 
      scanner-spec-flowlang
      grammar-flowlang)))

(interpretador)