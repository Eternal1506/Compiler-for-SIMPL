#lang racket

; Made by Jay Ren and Pratham Chauhan

(define (trans-op op)
  (match op
    ['+ 'add]
    ['- 'sub]
    ['* 'mul]
    ['div 'div]
    ['mod 'mod]
    ['< 'lt]
    ['<= 'le]
    ['> 'gt]
    ['>= 'ge]
    ['= 'equal]
    ['and 'land]
    ['or 'lor]
    ['not 'lnot]))

(define (compile-expr expr syms)
  (match expr
    [(? number?) `((move (0 __SP) ,expr) (add __SP __SP 1))]
    [(? boolean?) `((move (0 __SP) ,expr) (add __SP __SP 1))]
    ['true `((move (0 __SP) #t) (add __SP __SP 1))]
    ['false `((move (0 __SP) #f) (add __SP __SP 1))]
    [(? symbol?) `((move (0 __SP) ,(hash-ref syms expr)) (add __SP __SP 1))]
    [`(,binop ,expr1 ,expr2)
     (append (compile-expr expr1 syms)
             (compile-expr expr2 syms)
             `((sub __SP __SP 2) (,(trans-op binop) (0 __SP) (0 __SP) (1 __SP)) (add __SP __SP 1)))]
    [`(,unop ,expr)
     (append (compile-expr expr syms)
             `((sub __SP __SP 1) (,(trans-op unop) (0 __SP) (0 __SP)) (add __SP __SP 1)))]))

; Returns a list of A-PRIMPL statements
(define (compile-stmt stmt syms)
  (match stmt
    [`(print ,expr)
     (cond
       [(string? expr) `((print-string ,expr))]
       [else (append (compile-expr expr syms) `((sub __SP __SP 1) (print-val (0 __SP))))])]
    [`(iif ,expr ,stmt1 ,stmt2)
     (define TRUE_STMT (gensym 'TRUE_STMT))
     (define FALSE_STMT (gensym 'FALSE_STMT))
     (define DONE (gensym 'DONE))
     (append (compile-expr expr syms)
             `((sub __SP __SP 1) (branch (0 __SP) ,TRUE_STMT) (jump ,FALSE_STMT) (label ,TRUE_STMT))
             (compile-stmt stmt1 syms)
             `((jump ,DONE) (label ,FALSE_STMT))
             (compile-stmt stmt2 syms)
             `((label ,DONE)))]
    [`(while ,expr ,stmts ...)
     (define LOOP_START (gensym 'LOOP_START))
     (define LOOP_DONE (gensym 'LOOP_DONE))
     (define BODY_START (gensym 'BODY_START))
     (append `((label ,LOOP_START))
             (compile-expr expr syms)
             `((sub __SP __SP 1) (branch (0 __SP) ,BODY_START) (jump ,LOOP_DONE) (label ,BODY_START))
             (foldr (λ (stmt lst) (append (compile-stmt stmt syms) lst)) '() stmts)
             `((jump ,LOOP_START) (label ,LOOP_DONE)))]
    [`(set ,var ,expr)
     (append (compile-expr expr syms) `((sub __SP __SP 1)) `((move ,(hash-ref syms var) (0 __SP))))]
    [`(seq ,stmts ...) (foldr (λ (stmt lst) (append (compile-stmt stmt syms) lst)) '() stmts)]
    [`(skip) '()]))

(define (compile-sx-list sx-list acc syms)
  (if (empty? sx-list)
      acc
      (compile-sx-list (rest sx-list) (append (compile-stmt (first sx-list) syms) acc) syms)))

(define (compile-simpl sx)
  (match sx
    ; [`(vars) '()]
    [`(vars ,variables) '(0)]
    [`(vars ,variables ,stmts ...)
     (define syms (gensyms variables (hash)))
     (append (compile-sx-list stmts '() syms)
             (convert-vars-to-data-symbols variables syms)
             '(0 (data __SP __SPSTART) (label __SPSTART)))]))

(define (gensyms vars syms)
  (cond
    [(empty? vars) syms]
    [else (gensyms (rest vars) (hash-set syms (first (first vars)) (gensym (first (first vars)))))]))

(define (convert-vars-to-data-symbols vars syms)
  (map (λ (var) (list 'data (hash-ref syms (first var)) (second var))) vars))

(provide compile-simpl
         compile-expr)
