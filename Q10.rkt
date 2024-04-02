#lang racket


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
             `((sub __SP __SP 1) (,(trans-op unop) (0 __SP) (0 __SP)) (add __SP __SP 1)))]
		[`(,fun ,args ...) 'placeholder]))

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
    [`(skip) '()]
		[`(return ,expr) 'placeholder]))

(define (compile-fun fun)
	(match fun
		[`(fun (,name ,args ...) ,stmts ...) 'placeholder]))

(define (compile-simpl funs)
	'placeholder
	)
