#lang racket

(define-struct _func (num-args label))

(define (get-func-label fun funcs)
  (_func-label (hash-ref funcs fun))
  )
(define (get-func-num-args fun funcs)
  (_func-num-args (hash-ref funcs fun))
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
    ['not 'lnot]
    [_ #f]))

(define (push-stack value)
  `((move (0 SP) ,value) (add SP SP 1)))

(define (pop-stack amount)
  `((sub SP SP ,amount)))

(define (compile-expr expr syms funcs)
  (match expr
    [(? number?) (push-stack expr)]
    [(? boolean?) (push-stack expr)]
    ['true (push-stack #t)]
    ['false (push-stack #f)]
    [(? symbol?) (push-stack `(,(hash-ref syms expr) FP))]
    [`(,fun ,exprs ...)
     (cond
       [(symbol? (trans-op fun)) ;; If built-in function
        (match (cons fun exprs)
          [`(,binop ,expr1 ,expr2)
           (append (compile-expr expr1 syms funcs)
                   (compile-expr expr2 syms funcs)
                   (pop-stack 2)
                   `((,(trans-op binop) (0 SP) (0 SP) (1 SP)) (add SP SP 1)))]
          [`(,unop ,expr)
           (append (compile-expr expr syms funcs)
                   (pop-stack 1)
                   `((,(trans-op unop) (0 SP) (0 SP)) (add SP SP 1)))])]
       [else
        (define func-label (_func-label (hash-ref funcs fun)))
        (define func-num-args (_func-num-args (hash-ref funcs fun)))

        (if (not (= (length exprs) func-num-args)) (error "arguments") (void))

        (append
         (foldr (λ (expr result) (append (compile-expr expr syms funcs) result)) empty exprs)
         `(
           (jsr RETURN-ADDR ,func-label)
           (sub SP SP ,func-num-args))
         (push-stack 'RETURN-VAL))])]))

(define (compile-stmt stmt syms funcs)
  (match stmt
    [`(print ,expr)
     (cond
       [(string? expr) `((print-string ,expr))]
       [else (append (compile-expr expr syms funcs) `((sub SP SP 1) (print-val (0 SP))))])]
    [`(iif ,expr ,stmt1 ,stmt2)
     (define TRUE_STMT (gensym 'TRUE_STMT))
     (define FALSE_STMT (gensym 'FALSE_STMT))
     (define DONE (gensym 'DONE))
     (append (compile-expr expr syms funcs)
             `((sub SP SP 1) (branch (0 SP) ,TRUE_STMT) (jump ,FALSE_STMT) (label ,TRUE_STMT))
             (compile-stmt stmt1 syms funcs)
             `((jump ,DONE) (label ,FALSE_STMT))
             (compile-stmt stmt2 syms funcs)
             `((label ,DONE)))]
    [`(while ,expr ,stmts ...)
     (define LOOP_START (gensym 'LOOP_START))
     (define LOOP_DONE (gensym 'LOOP_DONE))
     (define BODY_START (gensym 'BODY_START))
     (append `((label ,LOOP_START))
             (compile-expr expr syms funcs)
             `((sub SP SP 1) (branch (0 SP) ,BODY_START) (jump ,LOOP_DONE) (label ,BODY_START))
             (foldr (λ (stmt lst) (append (compile-stmt stmt syms funcs) lst)) '() stmts)
             `((jump ,LOOP_START) (label ,LOOP_DONE)))]
    [`(set ,var ,expr)
     (append (compile-expr expr syms funcs) `((sub SP SP 1) (move ,(hash-ref syms var) (0 SP))))]
    [`(seq ,stmts ...) (foldr (λ (stmt lst) (append (compile-stmt stmt syms funcs) lst)) '() stmts)]
    [`(skip) '()]
    [`(return ,expr) (compile-expr expr syms funcs)]))

(define (gensyms vars syms)
  (cond
    [(empty? vars) syms]
    [else (gensyms (rest vars) (hash-set syms (first vars)
                                         ; (gensym (first vars))
                                         (first vars)
                                         ))]))

(define (compile-fun fun funcs)
  (match fun
    [`(fun (,name ,args ...) (vars [(,var-names ,init-vals) ...] ,stmts ...))
     (define num-args (length args))
     (define num-vars (length var-names))

     (define syms (gensyms (append args var-names) (hash)))

     ;  (printf "Compiling function ~a\n" name)
     ;  (display var-names) (newline)
     ;  (display init-vals) (newline) (newline)

     (define func-label (get-func-label name funcs))
     (define const-FP (str->s (str+ (s->str func-label) "_FP")))
     (define const-RETURN_ADDR (str->s (str+ (s->str func-label) "_RETURN_ADDR")))
     (define const-SIZE (str->s (str+ (s->str func-label) "_SIZE")))

     (define last-stmt (list-ref stmts (sub1 (length stmts))))
     ;; Error if the last statement is not a return statement
     (if (not (and (symbol? (first last-stmt)) (symbol=? 'return (first last-stmt))))
         (error "return") (void))

     (append
      ;; Offsets for the arguments
      (map (λ (arg offset) `(const ,(hash-ref syms arg) ,offset))
           args
           (build-list num-args (λ (n) (- n num-args))))
      `(
        (const ,const-FP 0)
        (const ,const-RETURN_ADDR 1))
      (map (λ (var offset) `(const ,(hash-ref syms var) ,offset))
           var-names
           (build-list num-vars (λ (n) (+ n 2))))
      `((const ,const-SIZE ,(+ 2 num-vars)))

      ;; Prologue
      `((label ,func-label)
        (move (,const-FP SP) FP) ; Save frame pointer of the caller
        (move (,const-RETURN_ADDR SP) RETURN-ADDR)) ;; Save return address

      (map (λ (var init-val) `(move (,(hash-ref syms var) SP) ,init-val))
           var-names
           init-vals)
      `((move FP SP)
        (add SP SP ,const-SIZE))

      ;; Body
      (foldr (λ (stmt acc) (append (compile-stmt stmt syms funcs) acc)) '() stmts)

      `((move RETURN-VAL (-1 SP))
        (sub SP SP 1))

      ;; Epilogue
      `((sub SP SP ,const-SIZE)
        (move FP (,const-FP SP))
        (move RETURN-ADDR (,const-RETURN_ADDR SP))
        (jump RETURN-ADDR))
      )]))

(define (check-for-duplicate-names funs namesSoFar)
  (cond
    [(empty? funs) (void)]
    [else
     (match (first funs)
       [`(fun (,name ,_ ...) ,_)
        (if (hash-ref namesSoFar name #f) (error "duplicate")
            (check-for-duplicate-names (rest funs) (hash-set namesSoFar name #t)))]
       [_ (error "Invalid function. This should never happen!")])]))

(define (get-function-details funs ht)
  (cond
    [(empty? funs) ht]
    [else
     (match (first funs)
       [`(fun (,name ,args ...) ,_)
        (get-function-details
         (rest funs)
         (hash-set ht name (_func (length args) (gensym name))))])])
  )

(define (compile-simpl funs)
  (check-for-duplicate-names funs (hash))
  (define funcs (get-function-details funs (hash)))
  (append
   `((jsr RETURN-ADDR ,(get-func-label 'main funcs))
     (halt))
   (foldr (λ (fun result) (append (compile-fun fun funcs) result)) empty funs)
   `((data RETURN-VAL 0)
     (data RETURN-ADDR 0)
     (data FP 0)
     (data SP END)
     (label END))))

; (compile-simpl
;  '(
;    (fun (f x y)
;         (vars [(i 10) (j 10)]
;               (set i (* (* j x) y))
;               (return (* i i))))
;    (fun (main)
;         (vars [(a 10) (b 10)]
;               (set a (* b (- 4 a)))
;               (print (f a b))
;               (return (- 4 4))))
;    ))

; (compile-simpl
;  '(
;    (fun (f x y)
;         (vars [(i 5)]
;               (return (+ (+ i x) y))))
;    (fun (main)
;         (vars [(a 10) (b 10)]
;               (print (f a b))
;               (return 0)))
;    ))

; (compile-simpl (list
;                  '(fun (fib n) (vars [] (iif (<= n 2)
;                                              (return 1)
;                                              (skip))
;                                      (return (+ (fib (- n 1)) (fib (- n 2))))))
;                  '(fun (main) (vars [(n 10) (cur 1)]
;                                     (while (and (and) (<= cur n))
;                                     (print (fib cur))
;                                     (set cur (+ 1 cur)))
;                                     (return 0)))
;                  ))

(provide compile-simpl)
