#lang racket

(define-struct _func (num-args label))

(define (get-func-label fun func-details)
  (_func-label (hash-ref func-details fun))
  )

(define str+ string-append)
(define s->str symbol->string)
(define str->s string->symbol)

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
    [(? number?) `((move (0 SP) ,expr) (add SP SP 1))]
    [(? boolean?) `((move (0 SP) ,expr) (add SP SP 1))]
    ['true `((move (0 SP) #t) (add SP SP 1))]
    ['false `((move (0 SP) #f) (add SP SP 1))]
    [(? symbol?) `((move (0 SP) ,(hash-ref syms expr)) (add SP SP 1))]
    [`(,binop ,expr1 ,expr2)
     (append (compile-expr expr1 syms)
             (compile-expr expr2 syms)
             `((sub SP SP 2) (,(trans-op binop) (0 SP) (0 SP) (1 SP)) (add SP SP 1)))]
    [`(,unop ,expr)
     (append (compile-expr expr syms)
             `((sub SP SP 1) (,(trans-op unop) (0 SP) (0 SP)) (add SP SP 1)))]
    [`(,fun ,args ...) 'placeholder]))

(define (compile-stmt stmt syms)
  (match stmt
    [`(print ,expr)
     (cond
       [(string? expr) `((print-string ,expr))]
       [else (append (compile-expr expr syms) `((sub SP SP 1) (print-val (0 SP))))])]
    [`(iif ,expr ,stmt1 ,stmt2)
     (define TRUE_STMT (gensym 'TRUE_STMT))
     (define FALSE_STMT (gensym 'FALSE_STMT))
     (define DONE (gensym 'DONE))
     (append (compile-expr expr syms)
             `((sub SP SP 1) (branch (0 SP) ,TRUE_STMT) (jump ,FALSE_STMT) (label ,TRUE_STMT))
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
             `((sub SP SP 1) (branch (0 SP) ,BODY_START) (jump ,LOOP_DONE) (label ,BODY_START))
             (foldr (λ (stmt lst) (append (compile-stmt stmt syms) lst)) '() stmts)
             `((jump ,LOOP_START) (label ,LOOP_DONE)))]
    [`(set ,var ,expr)
     (append (compile-expr expr syms) `((sub SP SP 1)) `((move ,(hash-ref syms var) (0 SP))))]
    [`(seq ,stmts ...) (foldr (λ (stmt lst) (append (compile-stmt stmt syms) lst)) '() stmts)]
    [`(skip) '()]
    [`(return ,expr) 'placeholder]))

(define (gensyms vars syms)
  (cond
    [(empty? vars) syms]
    [else (gensyms (rest vars) (hash-set syms (first (first vars)) (gensym (first (first vars)))))]))

(define (compile-fun fun func-details)
  (match fun
    [`(fun (,name ,args ...) (vars [,pairs ...] ,stmts ...))
     (define func-label (get-func-label name func-details))
     (define _FN_countdown_FP 0)
     (define _FN_countdown_RETURN-ADDR 1)
     (define _FN_countdown_SIZE (+ (length args) (length pairs)))

     (define last-stmt (list-ref stmts (sub1 (length stmts))))
     ;; Error if the last statement is not a return statement
     (if (not (and (symbol? (first last-stmt)) (symbol=? 'return (first last-stmt))))
         (error "return") (void))

     (append
      `((label ,func-label)
        (const ,(str->s (str+ func-label "_")) )
        ;; Prologue
        (move (,_FN_countdown_FP SP) FP) ; Save frame pointer of the caller
        (move (,_FN_countdown_RETURN-ADDR SP) RETURN-ADDR)) ;; Save return address
      ;; TODO: initialize variables here
      `((move FP SP)
        (add SP SP ,_FN_countdown_SIZE)
        )

      ;; Body
      ;; TODO: Compile every statement and put them here

      ;; TODO: Return the expression in last-stmt (this does not work yet)
      `((move RETURN-VAL (_FN_countdown_VAR_result FP)))

      ;; Epilogue
      `((sub SP SP ,_FN_countdown_SIZE)
        (move FP (,_FN_countdown_FP SP))
        (move RETURN-ADDR (,_FN_countdown_RETURN-ADDR SP))
        (jump RETURN-ADDR))
      )]))

(define (check-for-duplicate-names funs namesSoFar)
  (cond
    [(empty? funs) (void)]
    [else
     (match (first funs)
       [`(fun (,name _ ...) _ ...)
        (if (hash-ref namesSoFar name #f) (error "duplicate")
            (check-for-duplicate-names (rest funs) (hash-set name #t)))]
       [_ (error "Invalid function. This should never happen!")])]))

(define (get-function-details funs ht)
  (cond
    [(empty? funs) ht]
    [else
     (match (first funs)
       [`(fun (,name ,args ...) _)
        (get-function-details
         (rest funs)
         (hash-set ht name (_func (length args) (gensym name))))])])
  )

(define (compile-simpl funs)
  (check-for-duplicate-names funs (hash))
  (define func-details (get-function-details funs (hash)))
  (append
   (foldr (λ (fun result) (append (compile-fun fun func-details) result)) funs)
   `((data RETURN-VAL 0)
     (data RETURN-ADDR 0)
     (data FP 0)
     (data SP END)
     (label END))))

(provide compile-simpl)
