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
    (number ("-" digit (arbno digit)) number)
    (float (digit (arbno digit) "." digit (arbno digit)) number)
    (float ("-" digit (arbno digit) "." digit (arbno digit)) number)))

; Gramática simplificada y corregida
(define grammar-flowlang
  '((program (expression) a-program)
    
    ; Expresiones básicas
    (expression (number) lit-exp)
    (expression (string) str-exp)
    (expression (identifier) var-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    (expression ("vacio") empty-exp)
    (expression ("(" expression ")") paren-exp)
    
    ; Declaraciones y asignaciones
    (expression ("var" identifier "=" expression) var-decl-exp)
    (expression ("const" identifier "=" expression) const-decl-exp)
    (expression ("set" identifier "=" expression) assign-exp)
    
    ; Estructuras de control
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("while" expression "do" expression "done") while-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)
    (expression ("begin" expression (arbno expression) "end") begin-exp)
    
    ; Funciones
    (expression ("func" "(" (separated-list identifier ",") ")" "{" expression "}") func-exp)
    (expression ("return" expression) return-exp)
    
    ; Llamadas a funciones (usando una notación específica para evitar conflictos)
    (expression ("call" identifier "(" (separated-list expression ",") ")") func-call-exp)
    
    ; Listas
    (expression ("[" (separated-list expression ",") "]") list-exp)
    
    ; Primitivas (sin conflicto con llamadas regulares)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    
    ; Primitivas
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("%") mod-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("zero?") zero-test-prim)
    (primitive (">") greater-prim)
    (primitive ("<") less-prim)
    (primitive (">=") greater-equal-prim)
    (primitive ("<=") less-equal-prim)
    (primitive ("==") equal-prim)
    (primitive ("!=") not-equal-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)
    (primitive ("longitud") strlen-prim)
    (primitive ("concatenar") strcat-prim)
    (primitive ("vacio?") empty-test-prim)
    (primitive ("crear-lista") cons-prim)
    (primitive ("lista?") list-test-prim)
    (primitive ("cabeza") car-prim)
    (primitive ("cola") cdr-prim)
    (primitive ("append") append-prim)
    (primitive ("ref-list") list-ref-prim)
    (primitive ("set-list") list-set-prim)
    (primitive ("diccionario?") dict-test-prim)
    (primitive ("crear-diccionario") make-dict-prim)
    (primitive ("ref-diccionario") dict-ref-prim)
    (primitive ("set-diccionario") dict-set-prim)
    (primitive ("claves") keys-prim)
    (primitive ("valores") values-prim)))

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

; Definición de tipos para valores expresados
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (str-val (str string?))
  (list-val (lst list?))
  (dict-val (dict list?))
  (proc-val (proc proc?))
  (null-val))

; Definición de procedimientos
(define-datatype proc proc?
  (procedure
   (vars (list-of symbol?))
   (body expression?)
   (env environment?)))

; Definición de referencia para ambientes mutables
(define-datatype reference reference?
  (a-ref (pos integer?)
         (vec vector?)))

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