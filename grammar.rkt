#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLOWLANG
;; Venus Juliana Paipilla - 2343803
;; Daniel Arias Castrillón - 2222205
;; Proyecto Final - Fundamentos de Lenguajes de Programación
;; Link del git: https://github.com/IamVenusJulia/FlowLang.git
;; Profesor: Robinson Duque, Ph.D
;; Universidad del Valle, 2025
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. ESPECIFICACIÓN LÉXICA

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

;; 2. ESPECIFICACIÓN GRAMATICAL

(define especificacion-gramatical
  '((programa (expresion) un-programa)

    ; Binding auxiliar para var, const
    (binding (identificador "=" expresion) un-binding)
    
    ; Binding auxiliar para letrec
    (declaracion-letrec (identificador "(" (separated-list identificador ",") ")" "=" expresion) una-declaracion-letrec)
    
    ; --- Nuevo Nonterminal para Herencia Opcional ---
    (clausula-extends () no-extends) ; Opción 1: vacío
    (clausula-extends ("extends" identificador ";") con-extends) ; Opción 2: herencia

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
    
    ; Secuencia (Begin)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)

    ; Estructuras de Datos
    (expresion ("{" clausula-extends (separated-list binding ",") "}") record-exp)
    
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

    ; --- DEFINICIÓN COMPLETA DE PRIMITIVAS ---
    (primitiva ("+") prim-sum)
    (primitiva ("-") prim-sub)
    (primitiva ("*") prim-mul)
    (primitiva ("/") prim-div)
    (primitiva ("mod") prim-mod)
    (primitiva ("add1") prim-add1)
    (primitiva ("sub1") prim-sub1)
    (primitiva ("zero?") prim-zero?)
    (primitiva ("==") prim-equal)
    (primitiva ("<") prim-less)
    (primitiva (">") prim-greater)
    (primitiva ("<=") prim-lesseq)
    (primitiva (">=") prim-greatereq)
    (primitiva ("<>") prim-notequal)
    (primitiva ("and") prim-and)
    (primitiva ("or") prim-or)
    (primitiva ("not") prim-not)
    (primitiva ("longitud") prim-string-length)
    (primitiva ("concatenar") prim-string-append)
    (primitiva ("vacio") prim-empty)
    (primitiva ("vacio?") prim-empty?)
    (primitiva ("crear-lista") prim-cons)
    (primitiva ("lista?") prim-list?)
    (primitiva ("cabeza") prim-car)
    (primitiva ("cola") prim-cdr)
    (primitiva ("append") prim-append)
    (primitiva ("ref-list") prim-list-ref)
    (primitiva ("set-list") prim-list-set)
    (primitiva ("crear-diccionario") prim-make-dict)
    (primitiva ("diccionario?") prim-dict?)
    (primitiva ("ref-diccionario") prim-dict-ref)
    (primitiva ("set-diccionario") prim-dict-set)
    (primitiva ("claves") prim-dict-keys)
    (primitiva ("valores") prim-dict-values)
    (primitiva ("clone") prim-clone)
    (primitiva ("print") prim-print)
    (primitiva ("real") prim-real)
    (primitiva ("imag") prim-imag)
    ))

;; 3. GENERACIÓN AUTOMÁTICA DE DATATYPES (AST) Y PARSER

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; 4. DEFINICIÓN DE DATATYPES EN TIEMPO DE EJECUCIÓN Y REFERENCIAS

;; --- Referencias para manejo de const vs var ---
(define-datatype reference reference?
  (a-ref (position integer?) (vec vector?))
  (const-ref (value expval?)))

(define deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (vector-ref vec pos))
      (const-ref (val) val))))

(define setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec) (vector-set! vec pos val))
      (const-ref (v) (eopl:error 'setref! "No se puede modificar una constante")))))

;; --- Entorno actualizado para soportar refs ---
(define-datatype environment environment?
  (empty-env)
  ; Entorno mutable (para var)
  (extend-env
    (syms (list-of symbol?))
    (vals vector?) 
    (env environment?))
  ; Entorno inmutable (para const)
  (extend-env-const
    (syms (list-of symbol?))
    (vals (list-of expval?))
    (env environment?)))

(define extend-env-list
  (lambda (syms vals env)
    (extend-env syms (list->vector vals) env)))

(define extend-env-list-const
  (lambda (syms vals env)
    (extend-env-const syms vals env)))

;; Búsqueda de REFERENCIA (para set)
(define apply-env-ref
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
        (eopl:error 'apply-env "Variable no definida: ~s" search-sym))
      (extend-env (syms vals saved-env)
        (let ((pos (list-index syms search-sym)))
          (if pos
              (a-ref pos vals) ; Retorna referencia mutable
              (apply-env-ref saved-env search-sym))))
      (extend-env-const (syms vals saved-env)
        (let ((pos (list-index syms search-sym)))
          (if pos
              (const-ref (list-ref vals pos)) ; Retorna referencia constante
              (apply-env-ref saved-env search-sym)))))))

;; Búsqueda de VALOR (usando deref)
(define apply-env
  (lambda (env search-sym)
    (deref (apply-env-ref env search-sym))))

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
  (proto-val (fields vector?) ; Vector de pares (id . valor) - MUTABLE
             (env environment?) ; Entorno this (circular)
             (parent expval?)) ; Campo para el prototipo padre
  (proc-val (p proc?)))

;; 5. VALORES INICIALES

(define initial-env
  (lambda ()
    (extend-env-list-const ; Las constantes iniciales no deben cambiar
     '(true false null)
     (list (bool-val #t) (bool-val #f) (null-val))
     (empty-env))))

;; 6. FUNCIONES AUXILIARES

(define make-list
  (lambda (n val)
    (if (<= n 0) '() (cons val (make-list (- n 1) val)))))

(define un-binding-id
  (lambda (b) (cases binding b (un-binding (id exp) id))))

(define un-binding-exp
  (lambda (b) (cases binding b (un-binding (id exp) exp))))

(define list-set-func
  (lambda (lst idx val)
    (cond
      ((null? lst) '())
      ((zero? idx) (cons val (cdr lst)))
      (else (cons (car lst) (list-set-func (cdr lst) (- idx 1) val))))))

(define remove-assoc
  (lambda (key dict)
    (cond
      ((null? dict) '())
      ((equal? (car (car dict)) key) (remove-assoc key (cdr dict)))
      (else (cons (car dict) (remove-assoc key (cdr dict)))))))
      
; --- Lógica de Búsqueda de Campos con Delegación (Herencia) ---

(define find-field-in-object 
  (lambda (obj field-id)
    (cases expval obj
      (proto-val (fields-vec this-env parent)
        (let* ((fields-list (vector-ref fields-vec 0))
               (search (assoc field-id fields-list)))
          (if search
              (cdr search) ; Campo encontrado en este objeto
              
              ; Delegación: Si no se encuentra, buscar en el prototipo padre
              (cases expval parent
                (null-val () 
                  (eopl:error "Campo no encontrado: ~s" field-id))
                (else 
                  (find-field-in-object parent field-id))))))
      (else (eopl:error "find-field-in-object requiere un objeto proto-val")))))

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
      
      ; --- Literales ---
      (literal-numero (n) (num-val n))
      (literal-cadena (s) (string-val s))
      (literal-booleano-true () (bool-val #t))
      (literal-booleano-false () (bool-val #f))
      (literal-nulo () (null-val))
      
      ; IMPLEMENTACIÓN DE 'THIS'
      (literal-this () 
        (apply-env env 'this))

      ; --- Variables y Asignación ---
      (identificador-exp (first-id rest-ids)
        (let loop ((curr-val (apply-env env first-id)) (ids rest-ids))
          (if (null? ids)
              curr-val
              (cases expval curr-val
                (proto-val (fields-vec this-env parent)
                   (loop (find-field-in-object curr-val (car ids)) (cdr ids))) 
                (else (eopl:error "No es un objeto o campo inexistente en la cadena de prototipos: ~s" curr-val))))))

      (var-exp (bindings body)
        (let ((ids (map un-binding-id bindings))
              (vals (eval-rands (map un-binding-exp bindings) env)))
          (eval-expression body (extend-env-list ids vals env)))) ; Usa entorno mutable

      (const-exp (bindings body)
         (let ((ids (map un-binding-id bindings))
              (vals (eval-rands (map un-binding-exp bindings) env)))
          (eval-expression body (extend-env-list-const ids vals env)))) ; Usa entorno inmutable

      (set-exp (id exp)
        (let ((val (eval-expression exp env))
              (ref (apply-env-ref env id)))
          (setref! ref val)
          (void-val)))

      ; --- Control de Flujo ---
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
                  ; Simplificamos la comparación para tipos base
                  (if (equal? (expval->num test-val) (expval->num case-val)) 
                      (eval-expression (car rhs-list) env)
                      (loop (cdr lhs-list) (cdr rhs-list))))))))
      
      (begin-exp (first-exp rest-exps)
         (let loop ((val (eval-expression first-exp env)) (exps rest-exps))
           (if (null? exps)
               val
               (loop (eval-expression (car exps) env) (cdr exps)))))

      ; --- Funciones ---
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

      ; --- Estructuras de Datos (Record / Objeto con Herencia) ---
      (record-exp (parent-clause bindings) 
        (let* ((ids (map un-binding-id bindings))
               (vals-exps (map un-binding-exp bindings))
               
               ; 1. Obtener el prototipo padre
               (parent-val (cases clausula-extends parent-clause
                             (no-extends () (null-val))
                             (con-extends (parent-id) (apply-env env parent-id)))))
          
          ; 2. Crear un entorno con 'this' como una variable mutable (var)
          (let* ((env-with-this (extend-env-list '(this) (list (void-val)) env))
                 ; 3. Obtener la referencia mutable (a-ref) para 'this'
                 (this-ref (apply-env-ref env-with-this 'this)))
            
            ; 4. Crear el objeto con el entorno circular y el padre
            (let ((real-record-val (proto-val 
                                     (vector (make-list (length ids) '())) ; fields (placeholder)
                                     env-with-this ; env
                                     parent-val))) ; <-- Usar parent-val
              
              ; 5. Mutar el valor de la variable 'this' para que apunte al objeto recién creado
              (setref! this-ref real-record-val)
              
              ; 6. Evaluar las expresiones de campo/método usando el entorno ahora circular
              (let ((vals (eval-rands vals-exps env-with-this)))
                
                ; 7. Construir la lista final de campos/métodos
                (let ((fields-list (map cons ids vals)))
                  
                  ; 8. Actualizar el vector interno del objeto
                  (vector-set! (proto-val->fields real-record-val) 0 fields-list) 
                  
                  ; 9. Retornar el objeto final
                  real-record-val))))))

      (list-exp (exps)
        (list-val (eval-rands exps env)))
        
      (complejo-exp (r i)
        (let ((rv (eval-expression r env)) (iv (eval-expression i env)))
          (complex-val (expval->num rv) (expval->num iv))))

      ; --- Recursión (letrec) ---
      (letrec-exp (decls body)
        (let ((proc-names (map (lambda (decl) (cases declaracion-letrec decl (una-declaracion-letrec (id params body) id))) decls))
              (id-lists   (map (lambda (decl) (cases declaracion-letrec decl (una-declaracion-letrec (id params body) params))) decls))
              (bodies     (map (lambda (decl) (cases declaracion-letrec decl (una-declaracion-letrec (id params body) body))) decls)))
           
           (let* ((dummy-env (extend-env-list proc-names (make-list (length proc-names) (void-val)) env)))
             (let ((procs (map (lambda (ids bdy) (proc-val (procedure ids bdy dummy-env)))
                               id-lists bodies)))
               (let loop ((names proc-names) (vals procs))
                 (if (null? names)
                     #t
                     (begin
                       (setref! (apply-env-ref dummy-env (car names)) (car vals))
                       (loop (cdr names) (cdr vals)))))
               (eval-expression body dummy-env)))))

      ; --- Primitivas ---
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      
      (else (eopl:error "Expresión desconocida")))))

;; FUNCIONES DE EXTRACCIÓN
(define expval->num (lambda (v) (cases expval v (num-val (n) n) (else (eopl:error "Esperaba numero")))))
(define expval->bool (lambda (v) (cases expval v (bool-val (b) b) (else (eopl:error "Esperaba booleano")))))
(define expval->string (lambda (v) (cases expval v (string-val (s) s) (else (eopl:error "Esperaba cadena")))))
(define expval->list (lambda (v) (cases expval v (list-val (l) l) (else (eopl:error "Esperaba lista")))))
(define expval->complex (lambda (v) (cases expval v (complex-val (r i) (cons r i)) (else (eopl:error "Esperaba complejo")))))

(define proto-val->fields 
  (lambda (v) 
    (cases expval v (proto-val (fields env parent) fields) (else (eopl:error "Esperaba un objeto proto-val")))))

(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim
      ; Aritmética
      (prim-sum () (num-val (+ (expval->num (car args)) (expval->num (cadr args)))))
      (prim-sub () (num-val (- (expval->num (car args)) (expval->num (cadr args)))))
      (prim-mul () (num-val (* (expval->num (car args)) (expval->num (cadr args)))))
      (prim-div () (num-val (/ (expval->num (car args)) (expval->num (cadr args)))))
      (prim-mod () (num-val (modulo (expval->num (car args)) (expval->num (cadr args)))))
      (prim-add1 () (num-val (+ (expval->num (car args)) 1)))
      (prim-sub1 () (num-val (- (expval->num (car args)) 1)))
      
      ; Comparación
      (prim-zero? () (bool-val (zero? (expval->num (car args)))))
      (prim-equal () (bool-val (equal? (expval->num (car args)) (expval->num (cadr args)))))
      (prim-less () (bool-val (< (expval->num (car args)) (expval->num (cadr args)))))
      (prim-greater () (bool-val (> (expval->num (car args)) (expval->num (cadr args)))))
      (prim-lesseq () (bool-val (<= (expval->num (car args)) (expval->num (cadr args)))))
      (prim-greatereq () (bool-val (>= (expval->num (car args)) (expval->num (cadr args)))))
      (prim-notequal () (bool-val (not (equal? (expval->num (car args)) (expval->num (cadr args))))))
      
      ; Lógica
      (prim-and () (bool-val (and (expval->bool (car args)) (expval->bool (cadr args)))))
      (prim-or () (bool-val (or (expval->bool (car args)) (expval->bool (cadr args)))))
      (prim-not () (bool-val (not (expval->bool (car args)))))

      ; Cadenas
      (prim-string-length () (num-val (string-length (expval->string (car args)))))
      (prim-string-append () (string-val (string-append (expval->string (car args)) (expval->string (cadr args)))))

      ; Listas
      (prim-empty () (list-val '()))
      (prim-empty? () (bool-val (null? (expval->list (car args)))))
      (prim-cons () (list-val (cons (car args) (expval->list (cadr args)))))
      (prim-list? () (bool-val (cases expval (car args) (list-val (l) #t) (else #f))))
      (prim-car () (car (expval->list (car args))))
      (prim-cdr () (list-val (cdr (expval->list (car args)))))
      (prim-append () (list-val (append (expval->list (car args)) (expval->list (cadr args)))))
      (prim-list-ref () (list-ref (expval->list (car args)) (expval->num (cadr args))))
      (prim-list-set () (list-val (list-set-func (expval->list (car args)) (expval->num (cadr args)) (caddr args))))

      ; Diccionarios / Registros
      (prim-make-dict () (list-val '())) ; Diccionario representado como lista de pares
      (prim-dict? () (bool-val (cases expval (car args) (list-val (l) #t) (else #f)))) ; Simplificado
      (prim-dict-ref () 
         (let ((search (assoc (car args) (expval->list (cadr args))))) ; (clave, dict)
           (if search (cdr search) (null-val))))
      (prim-dict-set ()
         ; (clave, dict, valor)
         (let ((key (car args)) (dict (expval->list (cadr args))) (val (caddr args)))
           (list-val (cons (cons key val) (remove-assoc key dict)))))
      (prim-dict-keys () (list-val (map car (expval->list (car args)))))
      (prim-dict-values () (list-val (map cdr (expval->list (car args)))))

      ; Complejos
      (prim-real () (num-val (car (expval->complex (car args)))))
      (prim-imag () (num-val (cdr (expval->complex (car args)))))

      ; Varios
      (prim-clone () (car args)) ; Copia simple por valor (shallow)
      (prim-print () (begin (display (expval->printable (car args))) (newline) (void-val)))
      )))

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
      ; Implementación de impresión detallada de campos del objeto (similar a V1),
      ; pero conservando el indicador de herencia por prototipos.
      (proto-val (fields-vec env parent)
        (let ((fields (vector-ref fields-vec 0))) ; Obtener la lista de pares (id . valor)
          (string-append "{"
                         (if (null? fields)
                             ""
                             (let ((first-pair (car fields)))
                               (string-append (symbol->string (car first-pair)) ": " (expval->printable (cdr first-pair))
                                 (apply string-append
                                   (map (lambda (p)
                                          (string-append ", " (symbol->string (car p)) ": " (expval->printable (cdr p))))
                                        (cdr fields))))))
                         (cases expval parent ; Indicador de prototipo
                           (null-val () "")
                           (else " extends ..."))
                         "}")))
      (proc-val (p) "#<procedure>"))))

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

(define interpretador
  (sllgen:make-rep-loop "FLOWLANG -> " 
    (lambda (pgm) 
      (let ((result (eval-program pgm)))
        (display (expval->printable result))
        (newline)))
    (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)))

(interpretador)