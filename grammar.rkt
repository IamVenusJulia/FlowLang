#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAMÁTICA COMPLETA DE FLOWLANG
;; Venus Juliana Paipilla 2343803
;; Daniel Arias Castrillón 2222205
;; Proyecto Final - Fundamentos de Lenguajes de Programación
;; Profesor: Robinson Duque, Ph.D
;; Universidad del Valle, 2025
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define grammar-flowlang
  '((program (expression) a-program)

    ;;; ==================== EXPRESIONES BÁSICAS ====================
    (expression (number) lit-exp)
    (expression (string) str-exp)
    (expression (boolean) bool-exp)
    (expression ("null") null-exp)
    (expression (identifier) var-exp)

    ;;; ==================== DECLARACIONES Y ASIGNACIONES ====================
    (expression ("var" (arbno identifier "=" expression) ";") var-decl-exp)
    (expression ("const" (arbno identifier "=" expression) ";") const-decl-exp)
    (expression (identifier "=" expression) assign-exp)

    ;;; ==================== FUNCIONES ====================
    (expression ("func" "(" (separated-list identifier ",") ")" "{" (arbno expression) "return" expression "}") func-exp)
    (expression (identifier "(" (separated-list expression ",") ")") app-exp)

    ;;; ==================== ESTRUCTURAS DE CONTROL ====================
    (expression ("if" expression "then" expression "else" expression "end") if-exp)
    (expression ("switch" expression "{" (arbno "case" expression ":" expression) "default" ":" expression "}") switch-exp)
    (expression ("while" expression "do" expression "done") while-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)
    (expression ("begin" (arbno expression ";") "end") begin-exp)

    ;;; ==================== ESTRUCTURAS DE DATOS ====================
    ; Listas
    (expression ("[" (separated-list expression ",") "]") list-exp)
    (expression ("vacio") empty-list-exp)
    
    ; Diccionarios
    (expression ("{" (separated-list identifier ":" expression ",") "}") dict-exp)

    ;;; ==================== PRIMITIVAS BOOLEANAS ====================
    (expression (pred-prim "(" expression "," expression ")") pred-prim-exp)
    (expression (oper-bin-bool "(" expression "," expression ")") bool-bin-exp)
    (expression (oper-un-bool "(" expression ")") bool-un-exp)

    (pred-prim ("<") less-prim)
    (pred-prim (">") greater-prim)
    (pred-prim ("<=") less-equal-prim)
    (pred-prim (">=") greater-equal-prim)
    (pred-prim ("==") equal-prim)
    (pred-prim ("<>") not-equal-prim)

    (oper-bin-bool ("and") and-prim)
    (oper-bin-bool ("or") or-prim)
    (oper-un-bool ("not") not-prim)

    ;;; ==================== PRIMITIVAS ARITMÉTICAS ====================
    (expression (arithmetic-prim "(" (separated-list expression ",") ")") arith-prim-exp)
    
    (arithmetic-prim ("+") add-prim)
    (arithmetic-prim ("-") substract-prim)
    (arithmetic-prim ("*") mult-prim)
    (arithmetic-prim ("/") div-prim)
    (arithmetic-prim ("%") mod-prim)
    (arithmetic-prim ("add1") incr-prim)
    (arithmetic-prim ("sub1") decr-prim)

    ;;; ==================== PRIMITIVAS DE CADENAS ====================
    (expression (string-prim "(" (separated-list expression ",") ")") str-prim-exp)
    
    (string-prim ("longitud") strlen-prim)
    (string-prim ("concatenar") strcat-prim)

    ;;; ==================== PRIMITIVAS DE LISTAS ====================
    (expression (list-prim "(" (separated-list expression ",") ")") list-prim-exp)
    
    (list-prim ("vacio?") empty-list?-prim)
    (list-prim ("crear-lista") cons-list-prim)
    (list-prim ("lista?") list?-prim)
    (list-prim ("cabeza") head-list-prim)
    (list-prim ("cola") tail-list-prim)
    (list-prim ("append") append-list-prim)
    (list-prim ("ref-list") ref-list-prim)
    (list-prim ("set-list") set-list-prim)

    ;;; ==================== PRIMITIVAS DE DICCIONARIOS ====================
    (expression (dict-prim "(" (separated-list expression ",") ")") dict-prim-exp)
    
    (dict-prim ("diccionario?") dict?-prim)
    (dict-prim ("crear-diccionario") make-dict-prim)
    (dict-prim ("ref-diccionario") ref-dict-prim)
    (dict-prim ("set-diccionario") set-dict-prim)
    (dict-prim ("claves") keys-dict-prim)
    (dict-prim ("valores") values-dict-prim)

    ;;; ==================== ENTRADA/SALIDA ====================
    (expression ("print" "(" expression ")") print-exp)

    ;;; ==================== TIPOS PARA FUTURA EXTENSIÓN ====================
    (type-exp ("int") int-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("string") string-type-exp)
    (type-exp ("list") list-type-exp)
    (type-exp ("dict") dict-type-exp)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")") proc-type-exp)
    ))

; Especificación léxica para FlowLang
(define scanner-spec-flowlang
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (comment ("/*" (arbno (not "*/")) "*/") skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (string ("\"" (arbno (or (not #\") "\\\"")) "\"") string)
    (boolean ("true") #t)
    (boolean ("false") #f)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)))