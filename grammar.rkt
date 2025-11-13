#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAMÁTICA COMPLETA DE FLOWLANG
;; Venus Juliana Paipilla y Daniel Arias Castrillón
;; Proyecto Final - Fundamentos de Lenguajes de Programación
;; Profesor: Robinson Duque, Ph.D
;; Universidad del Valle, 2025
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ==========================================================
;; ESPECIFICACIÓN LÉXICA
;; ==========================================================

(define flowlang-lexical-spec
  '(
    ;; Ignorar espacios y comentarios
    (white-space (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)

    ;; Tokens básicos
    (identifier (letter (arbno (or letter digit "_"))) symbol)
    (number ((opt "-") (digit (arbno digit)) (opt "." (digit (arbno digit)))) number)
    (string ("\"" (arbno (or (not #\") (seq #\\ #\"))) "\"") string)
    (boolean ("true" | "false") symbol)

    ;; Palabras reservadas
    (keyword ("var" | "const" | "func" | "if" | "then" | "else" | "end"
              | "while" | "do" | "done" | "for" | "in" | "switch"
              | "case" | "default" | "begin" | "return" | "null"
              | "prototipo" | "clone" | "print") symbol)
))

;; ==========================================================
;; ESPECIFICACIÓN GRAMATICAL
;; ==========================================================

(define flowlang-grammar
  '(
    ;; Programa raíz
    (program (expression) a-program)

    ;; ==========================
    ;; EXPRESIONES BÁSICAS
    ;; ==========================
    (expression (identifier) var-exp)
    (expression (number) num-exp)
    (expression (string) str-exp)
    (expression (boolean) bool-exp)
    (expression ("null") null-exp)

    ;; ==========================
    ;; DECLARACIONES
    ;; ==========================
    (expression ("var" (separated-list var-assign ",") ";" expression)
                var-decl-exp)
    (expression ("const" (separated-list var-assign ",") ";" expression)
                const-decl-exp)
    (expression (identifier ":=" expression)
                assign-exp)

    ;; ==========================
    ;; FUNCIONES
    ;; ==========================
    (expression ("func" identifier "(" (separated-list identifier ",") ")" 
                 "{" (arbno expression) "return" expression "}")
                func-exp)
    (expression (identifier "(" (separated-list expression ",") ")")
                call-exp)

    ;; ==========================
    ;; ESTRUCTURAS DE CONTROL
    ;; ==========================
    (expression ("if" expression "then" expression "else" expression "end")
                if-exp)

    (expression ("switch" expression "{" (arbno case-clause)
                 (default-clause) "}")
                switch-exp)
    
    (case-clause ("case" expression ":" expression)
                 a-case-clause)
    
    (default-clause ("default" ":" expression)
                    a-default-clause)

    (expression ("while" expression "do" expression "done")
                while-exp)

    (expression ("for" identifier "in" expression "do" expression "done")
                for-exp)

    ;; ==========================
    ;; SECUENCIAS
    ;; ==========================
    (expression ("begin" (separated-list expression ";") "end")
                begin-exp)

    ;; ==========================
    ;; LISTAS Y DICCIONARIOS
    ;; ==========================
    (expression ("[" (separated-list expression ",") "]")
                list-exp)
    (expression ("{" (separated-list dict-pair ",") "}")
                dict-exp)

    ;; ==========================
    ;; PROTOTIPOS Y OBJETOS
    ;; ==========================
    (expression ("prototipo" identifier "=" expression)
                proto-decl-exp)
    (expression ("clone" "(" expression ")")
                clone-exp)

    ;; ==========================
    ;; OPERACIONES ARITMÉTICAS / BOOLEANAS
    ;; ==========================
    (expression ("(" expression binop expression ")")
                binop-exp)
    (expression ("(" unop expression ")")
                unop-exp)

    ;; ==========================
    ;; IMPRESIÓN
    ;; ==========================
    (expression ("print" "(" expression ")")
                print-exp)

    ;; ==========================
    ;; AUXILIARES
    ;; ==========================
    (var-assign (identifier "=" expression) a-var-assign)
    (dict-pair (identifier ":" expression) a-dict-pair)
    
    (binop ("+" | "-" | "*" | "/" | "%" 
            | "<" | ">" | "<=" | ">=" | "==" | "<>" 
            | "and" | "or") a-binop)
    
    (unop ("not" | "add1" | "sub1") a-unop)
))

;; ==========================================================
;; SCAN & PARSE
;; ==========================================================

(sllgen:make-define-datatypes flowlang-lexical-spec flowlang-grammar)

(define scan&parse
  (sllgen:make-string-parser flowlang-lexical-spec flowlang-grammar))

;; ==========================================================
;; GENERADOR DE LÉXICO Y PARSER AUTOMÁTICO
;; ==========================================================

(define just-scan
  (sllgen:make-string-scanner flowlang-lexical-spec flowlang-grammar))

;; ==========================================================
;; EJEMPLOS DE PRUEBA
;; ==========================================================

;; Pruebas corregidas:
;; (scan&parse "var x = 10; print(x)")
;; (scan&parse "const y = 5, z = 9; print(y+z)")
;; (scan&parse "if x > y then print(\"mayor\") else print(\"menor\") end")
;; (scan&parse "func sumar(a,b){ return (a + b) }")
;; (scan&parse "for i in [1,2,3] do print(i) done")
;; (scan&parse "{nombre:\"Ana\", edad:25}")
;; (scan&parse "prototipo persona = {nombre:\"Ana\", edad:25}")
;; (scan&parse "clone(persona)")
;; (scan&parse "switch x { case 1: print(\"uno\") case 2: print(\"dos\") default: print(\"otro\") }")
;; (scan&parse "begin var x = 1; var y = 2; print((x + y)); end")
