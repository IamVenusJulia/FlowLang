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

;Gramática

(define especificacion-gramatical
  '((programa (expresion) un-programa)

    ; Binding auxiliar para var, const
    (binding (identificador "=" expresion) un-binding)
    
    ; Binding auxiliar para letrec
    (declaracion-letrec (identificador "(" (separated-list identificador ",") ")" "=" expresion) una-declaracion-letrec)

    ; Expresiones Básicas
    (expresion (numero) literal-numero)
    (expresion (cadena) literal-cadena)
    (expresion ("true") literal-booleano-true)
    (expresion ("false") literal-booleano-false)
    (expresion ("null") literal-nulo)
    (expresion ("this") literal-this)

    ; Identificadores y Acceso (a.b.c)
    (expresion (identificador (arbno "." identificador)) identificador-exp)

    ; Declaraciones
    (expresion ("var" (separated-list binding ",") "in" expresion) var-exp)
    (expresion ("const" (separated-list binding ",") "in" expresion) const-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ; Control de Flujo
    (expresion ("if" expresion "then" expresion "else" expresion "end") if-exp)
    
    ; Switch
    (expresion ("switch" expresion 
                (arbno "case" expresion ":" expresion) 
                "default" ":" expresion "end") 
               switch-exp)
               
    (expresion ("while" expresion "do" expresion "done") while-exp)
    
    (expresion ("for" identificador "in" expresion "do" expresion "done") for-exp)

    ; Estructuras de Datos
    (expresion ("{" (separated-list binding ",") "}") record-exp)
    (expresion ("[" (separated-list expresion ",") "]") list-exp)
    (expresion ("complejo" "(" expresion "," expresion ")") complejo-exp)

    ; Primitivas
    (expresion (primitiva "(" (separated-list expresion ",") ")") primapp-exp)
    
    ; Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("(" expresion (arbno expresion) ")") func-call-exp)
    (expresion ("return" expresion) return-exp)

    ; Recursión (letrec)
    (expresion ("letrec" (separated-list declaracion-letrec ";") "in" expresion) letrec-exp)

    ; Definición de Primitivas Explícitas
    (primitiva ("+") prim-sum)
    (primitiva ("-") prim-sub)
    (primitiva ("*") prim-mul)
    (primitiva ("/") prim-div)
    (primitiva ("mod") prim-mod)
    (primitiva ("add1") prim-add1)
    (primitiva ("sub1") prim-sub1)
    (primitiva ("print") prim-print)
    (primitiva ("==") prim-equal)
    (primitiva ("<") prim-less)
    (primitiva (">") prim-greater)
    (primitiva ("<=") prim-lesseq)
    (primitiva (">=") prim-greatereq)
    (primitiva ("<>") prim-notequal)
    (primitiva ("and") prim-and)
    (primitiva ("or") prim-or)
    (primitiva ("not") prim-not)
    ))

; ==================== GENERAR TIPOS DE DATOS ====================

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

; ==================== IMPLEMENTACIÓN DEL INTERPRETADOR ====================

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (syms (list-of symbol?))
    (vals vector?) 
    (env environment?)))

(define extend-env-list
  (lambda (syms vals env)
    (extend-env syms (list->vector vals) env)))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (syms vals saved-env)
        (let ((pos (list-index syms search-sym)))
          (if pos
              (vector-ref vals pos)
              (apply-env saved-env search-sym)))))))

(define set-env!
  (lambda (env search-sym new-val)
    (cases environment env
      (empty-env ()
        (eopl:error 'set-env! "No binding for ~s" search-sym))
      (extend-env (syms vals saved-env)
        (let ((pos (list-index syms search-sym)))
          (if pos
              (vector-set! vals pos new-val)
              (set-env! saved-env search-sym new-val)))))))

(define list-index
  (lambda (syms sym)
    (let loop ((lst syms) (pos 0))
      (cond
        ((null? lst) #f)
        ((eqv? (car lst) sym) pos)
        (else (loop (cdr lst) (+ pos 1)))))))

(define-datatype proc proc?
  (procedure
   (params (list-of symbol?))
   (body expresion?)
   (env environment?)))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (string-val (str string?))
  (null-val)
  (void-val)
  (complex-val (real number?) (imag number?))
  (list-val (lst (list-of expval?)))
  (proto-val (fields vector?)) 
  (proc-val (p proc?)))

; Ambiente inicial

(define initial-env
  (lambda ()
    (extend-env-list
     '(true false null)
     (list (bool-val #t) (bool-val #f) (null-val))
     (empty-env))))

(define prim-proc
  (lambda (op)
    (proc-val (procedure 'args (literal-nulo) (empty-env))))) 

;; 6. FUNCIONES AUXILIARES PARA EL INTÉRPRETE

; Función manual make-list para evitar dependencias externas
(define make-list
  (lambda (n val)
    (if (<= n 0)
        '()
        (cons val (make-list (- n 1) val)))))

(define un-binding-id
  (lambda (b)
    (cases binding b
      (un-binding (id exp) id))))

(define un-binding-exp
  (lambda (b)
    (cases binding b
      (un-binding (id exp) exp))))

;; 7. INTÉRPRETE (EVALUADOR)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp) (eval-expression exp (initial-env))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (eval-expression rand env)) rands)))

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      
      ; Literales
      (literal-numero (n) (num-val n))
      (literal-cadena (s) (string-val s))
      (literal-booleano-true () (bool-val #t))
      (literal-booleano-false () (bool-val #f))
      (literal-nulo () (null-val))
      (literal-this () (eopl:error "uso de 'this' fuera de contexto"))

      ; Variables y Asignación 
      (identificador-exp (first-id rest-ids)
        (let loop ((curr-val (apply-env env first-id)) (ids rest-ids))
          (if (null? ids)
              curr-val
              (cases expval curr-val
                (proto-val (vec)
                   (let* ((fields (vector-ref vec 0))
                          (search (assoc (car ids) fields)))
                     (if search
                         (loop (cdr search) (cdr ids))
                         (eopl:error "Campo no encontrado:" (car ids)))))
                (else (eopl:error "No es un objeto:" curr-val))))))

      (var-exp (bindings body)
        (let ((ids (map un-binding-id bindings))
              (vals (eval-rands (map un-binding-exp bindings) env)))
          (eval-expression body (extend-env-list ids vals env))))

      (const-exp (bindings body)
         (let ((ids (map un-binding-id bindings))
              (vals (eval-rands (map un-binding-exp bindings) env)))
          (eval-expression body (extend-env-list ids vals env))))

      (set-exp (id exp)
        (let ((val (eval-expression exp env)))
          (set-env! env id val)
          (void-val)))

      ; Control de Flujo
      (if-exp (test then else-exp)
        (let ((val (eval-expression test env)))
          (cases expval val
            (bool-val (b) (if b (eval-expression then env) (eval-expression else-exp env)))
            (else (eopl:error "If requiere booleano")))))

      (while-exp (test body)
        (let loop ()
          (let ((val (eval-expression test env)))
            (cases expval val
              (bool-val (b)
                (if b (begin (eval-expression body env) (loop)) (void-val)))
              (else (eopl:error "While requiere booleano"))))))
      
      (for-exp (id list-exp body)
        (let ((lst-val (eval-expression list-exp env)))
          (cases expval lst-val
            (list-val (lst)
              (for-each (lambda (elem)
                          (eval-expression body (extend-env-list (list id) (list elem) env)))
                        lst)
              (void-val))
            (else (eopl:error "For requiere una lista")))))

      (switch-exp (test cases-lhs cases-rhs default)
        (let ((test-val (eval-expression test env)))
          (let loop ((lhs-list cases-lhs) (rhs-list cases-rhs))
            (if (null? lhs-list)
                (eval-expression default env)
                (let ((case-val (eval-expression (car lhs-list) env)))
                  (if (equal? (expval->num test-val) (expval->num case-val))
                      (eval-expression (car rhs-list) env)
                      (loop (cdr lhs-list) (cdr rhs-list))))))))

      ; Funciones
      (func-exp (params body)
        (proc-val (procedure params body env)))

      (func-call-exp (rater rands)
        (let ((proc-value (eval-expression rater env))
              (args (eval-rands rands env)))
          (cases expval proc-value
            (proc-val (p)
              (cases proc p
                (procedure (params body saved-env)
                  (if (= (length params) (length args))
                      (eval-expression body (extend-env-list params args saved-env))
                      (eopl:error "Num argumentos incorrecto")))))
            (else (eopl:error "No es un procedimiento")))))

      (return-exp (exp) (eval-expression exp env))

      ; Estructuras
      (list-exp (exps)
        (list-val (eval-rands exps env)))

      (record-exp (bindings)
        (let ((ids (map un-binding-id bindings))
              (vals (eval-rands (map un-binding-exp bindings) env)))
          (proto-val (vector (map cons ids vals)))))

      (complejo-exp (r i)
        (let ((rv (eval-expression r env)) (iv (eval-expression i env)))
          (complex-val (expval->num rv) (expval->num iv))))

      ; Recursión (letrec)
      (letrec-exp (decls body)
        (let ((proc-names (map (lambda (decl) (cases declaracion-letrec decl (una-declaracion-letrec (id params body) id))) decls))
              (id-lists   (map (lambda (decl) (cases declaracion-letrec decl (una-declaracion-letrec (id params body) params))) decls))
              (bodies     (map (lambda (decl) (cases declaracion-letrec decl (una-declaracion-letrec (id params body) body))) decls)))
           
           (let* ((dummy-env (extend-env-list proc-names (make-list (length proc-names) (void-val)) env)))
             (let ((procs (map (lambda (ids bdy) (proc-val (procedure ids bdy dummy-env)))
                               id-lists bodies)))
               (for-each (lambda (name val) (set-env! dummy-env name val)) proc-names procs)
               (eval-expression body dummy-env)))))

      ; Primitivas
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      
      (else (eopl:error "Expresión desconocida")))))

; Función auxiliar para valores booleanos y numéricos
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (n) n)
      (else (eopl:error "Esperaba numero")))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (b) b)
      (else (eopl:error "Esperaba booleano")))))

(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim
      (prim-sum () (num-val (+ (expval->num (car args)) (expval->num (cadr args)))))
      (prim-sub () (num-val (- (expval->num (car args)) (expval->num (cadr args)))))
      (prim-mul () (num-val (* (expval->num (car args)) (expval->num (cadr args)))))
      (prim-div () (num-val (/ (expval->num (car args)) (expval->num (cadr args)))))
      (prim-mod () (num-val (modulo (expval->num (car args)) (expval->num (cadr args)))))
      (prim-add1 () (num-val (+ (expval->num (car args)) 1)))
      (prim-sub1 () (num-val (- (expval->num (car args)) 1)))
      (prim-print () (begin (display (expval->printable (car args))) (newline) (void-val)))
      (prim-equal () (bool-val (equal? (expval->num (car args)) (expval->num (cadr args)))))
      (prim-less () (bool-val (< (expval->num (car args)) (expval->num (cadr args)))))
      (prim-greater () (bool-val (> (expval->num (car args)) (expval->num (cadr args)))))
      (prim-lesseq () (bool-val (<= (expval->num (car args)) (expval->num (cadr args)))))
      (prim-greatereq () (bool-val (>= (expval->num (car args)) (expval->num (cadr args)))))
      (prim-notequal () (bool-val (not (equal? (expval->num (car args)) (expval->num (cadr args))))))
      (prim-and () (bool-val (and (expval->bool (car args)) (expval->bool (cadr args)))))
      (prim-or () (bool-val (or (expval->bool (car args)) (expval->bool (cadr args)))))
      (prim-not () (bool-val (not (expval->bool (car args))))))))

;; 8. IMPRESIÓN Y REPL

(define expval->printable
  (lambda (val)
    (cases expval val
      (num-val (n) (number->string n))
      (bool-val (b) (if b "true" "false"))
      (string-val (s) (string-append "\"" s "\""))
      (null-val () "null")
      (void-val () "void")
      (list-val (lst)
        (string-append "["
                       (if (null? lst)
                           ""
                           (let ((first (expval->printable (car lst))))
                             (string-append first
                                            (apply string-append
                                                   (map (lambda (v)
                                                          (string-append ", " (expval->printable v)))
                                                        (cdr lst))))))
                       "]"))
      (complex-val (r i) (string-append (number->string r) "+" (number->string i) "i"))
      (proto-val (v) "{objeto}")
      (proc-val (p) "#<procedure>"))))

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

; Interpretador (REPL)
(define interpretador
  (sllgen:make-rep-loop "FLOWLANG -> " 
    (lambda (pgm) 
      (let ((result (eval-program pgm)))
        (display (expval->printable result))
        (newline)))
    (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)))

(interpretador)