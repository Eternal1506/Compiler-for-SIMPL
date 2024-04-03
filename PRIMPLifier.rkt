#lang racket

;; Made by: Jay Ren and Pratham Chauhan

(define-struct symbols-result (consts labels datas)
  #:transparent)

(define (error-if-duplicate name consts labels datas)
  (if (and (boolean? (hash-ref consts name #f))
           (boolean? (hash-ref labels name #f))
           (boolean? (hash-ref datas name #f)))
      (void)
      (error "duplicate")))

; list -> symbols-result
; I think this works.
(define (extract-symbols instructions consts labels datas counter)
  (cond
    [(empty? instructions) (symbols-result consts labels datas)]
    [else
     (match (first instructions)
       [`(data ,name (,nat ,_))
        (error-if-duplicate name consts labels datas)
        (extract-symbols (rest instructions)
                         consts
                         labels
                         (hash-set datas name counter)
                         (+ counter nat))]
       [`(data ,name ,many-symbols ...)
        (error-if-duplicate name consts labels datas)
        (extract-symbols (rest instructions)
                         consts
                         labels
                         (hash-set datas name counter)
                         (+ counter (length many-symbols)))]
       [`(const ,name ,symbol)
        (error-if-duplicate name consts labels datas)
        (extract-symbols (rest instructions) (hash-set consts name symbol) labels datas counter)]
       [`(label ,name)
        (error-if-duplicate name consts labels datas)
        (extract-symbols (rest instructions) consts (hash-set labels name counter) datas counter)]
       [_ (extract-symbols (rest instructions) consts labels datas (add1 counter))])]))

(define (detect-duplicates consts)
  (map (λ (key) (detect-duplicates-h key consts (hash))) (hash-keys consts)))
(define (detect-duplicates-h key consts visited)
  (define already-visited? (hash-ref visited key #f))
  (cond
    [already-visited? (error "circular")]
    [(or (number? key) (boolean? key)) (void)]
    [(hash-has-key? consts key)
     (detect-duplicates-h (hash-ref consts key) consts (hash-set visited key #t))]
    [else (void)]))

(define (assemble instructions consts labels datas acc)
  (cond
    [(empty? instructions) acc]
    [else
     (match (first instructions)
       [`(data ,_ (,nat ,symbol)) ; this works
        (define val (resolve-imm symbol consts labels datas))
        (assemble (rest instructions) consts labels datas (append (build-list nat (λ (_) val)) acc))]
       [(list 'data _ many-symbols ...) ; this works
        (assemble (rest instructions)
                  consts
                  labels
                  datas
                  (append (map (λ (s) (resolve-imm s consts labels datas)) (reverse many-symbols))
                          acc))]
       [`(const ,_ ,_) (assemble (rest instructions) consts labels datas acc)] ; this works
       [`(label ,_) (assemble (rest instructions) consts labels datas acc)] ; this works
       ; this works
       [`(halt) (assemble (rest instructions) consts labels datas (cons 0 acc))]
       [`(jump ,targ)
        (define resolved-targ (resolve-targ targ consts labels datas))
        (assemble (rest instructions)
                  consts
                  labels
                  datas
                  (cons (list 'jump resolved-targ) acc))] ; this works I think
       [`(branch ,opd ,targ) ; this works I think
        (define resolved-opd (resolve-opd opd consts labels datas))
        (define resolved-targ (resolve-targ targ consts labels datas))
        (assemble (rest instructions)
                  consts
                  labels
                  datas
                  (cons (list 'branch resolved-opd resolved-targ) acc))]
       [`(print-string ,str) ; this works
        (assemble (rest instructions) consts labels datas (cons (list 'print-string str) acc))]
       [`(print-val ,opd) ; this works, probably
        (define resolved-opd (resolve-opd opd consts labels datas))
        (assemble (rest instructions) consts labels datas (cons (list 'print-val resolved-opd) acc))]
       [`(,operation ,dest ,opd1 ,opd2)
        (define resolved-dest (resolve-dest dest consts labels datas))
        (define resolved-opd1 (resolve-opd opd1 consts labels datas))
        (define resolved-opd2 (resolve-opd opd2 consts labels datas))
        (assemble (rest instructions)
                  consts
                  labels
                  datas
                  (cons (list operation resolved-dest resolved-opd1 resolved-opd2) acc))]
       [`(,operation ,dest ,opd)
        (define resolved-dest (resolve-dest dest consts labels datas))
        (define resolved-opd (resolve-opd opd consts labels datas))
        (assemble (rest instructions)
                  consts
                  labels
                  datas
                  (cons (list operation resolved-dest resolved-opd) acc))]
       [x
        (assemble (rest instructions)
                  consts
                  labels
                  datas
                  (cons (resolve-imm x consts labels datas) acc))])]))

(define (resolve-targ targ consts labels datas)
  (match targ
    [(? number?) targ]
    [(? symbol?)
     (cond
       [(hash-has-key? datas targ) (list (resolve-imm targ consts labels datas))]
       [(hash-has-key? consts targ) (list (resolve-imm targ consts labels datas))]
       [(hash-has-key? labels targ) (resolve-imm targ consts labels datas)] ; wrap in a list
       [else (error "undefined")])]
    [`(,nat) (list nat)]
    [`(,imm ,ind)
     (list (resolve-imm imm consts labels datas) (resolve-dest ind consts labels datas))]))

(define (resolve-dest dest consts labels datas) ; most likely works
  (match dest
    [(? symbol?)
     (cond
       [(hash-has-key? datas dest) (list (resolve-imm dest consts labels datas))] ; wrap in a list
       [(hash-has-key? consts dest) (error "incorrect")]
       [(hash-has-key? labels dest) (error "incorrect")]
       [else (error "undefined")])]
    [`(,nat) (list nat)]
    [`(,imm ,ind) (list (resolve-imm imm consts labels datas) (resolve-dest ind consts labels datas))]
    [_ (error "incorrect")]))

(define (resolve-opd opd consts labels datas)
  (match opd
    [(? number?) opd]
    [(? boolean?) opd]
    [(? symbol?)
     (cond
       [(hash-has-key? datas opd) (list (resolve-imm opd consts labels datas))] ; wrap in list
       [(hash-has-key? consts opd) (resolve-imm opd consts labels datas)]
       [(hash-has-key? labels opd) (resolve-imm opd consts labels datas)]
       [else (error "undefined")])]
    [`(,nat) (list nat)]
    [`(,psymbol-or-imm ,psymbol-or-ind)
     (list (resolve-imm psymbol-or-imm consts labels datas)
           (resolve-opd psymbol-or-ind consts labels datas))]))

; This is essentially getting the address of a symbol
(define (resolve-imm psymbol-or-value consts labels datas)
  (resolve-imm-h psymbol-or-value consts labels datas (hash)))
(define (resolve-imm-h psymbol-or-value consts labels datas visited)
  (define already-visited? (hash-ref visited psymbol-or-value #f))
  (cond
    [(number? psymbol-or-value) psymbol-or-value]
    [(boolean? psymbol-or-value) psymbol-or-value]
    [already-visited? (error "circular")]
    [(hash-has-key? consts psymbol-or-value)
     (resolve-imm-h (hash-ref consts psymbol-or-value)
                    consts
                    labels
                    datas
                    (hash-set visited psymbol-or-value #t))]
    [(hash-has-key? labels psymbol-or-value)
     (resolve-imm-h (hash-ref labels psymbol-or-value)
                    consts
                    labels
                    datas
                    (hash-set visited psymbol-or-value #t))]
    [(hash-has-key? datas psymbol-or-value)
     (resolve-imm-h (hash-ref datas psymbol-or-value)
                    consts
                    labels
                    datas
                    (hash-set visited psymbol-or-value #t))]
    [else (display psymbol-or-value) (error "undefined")]))

(define (primplify instructions)
  (define result
    (extract-symbols instructions
                     (make-immutable-hash)
                     (make-immutable-hash)
                     (make-immutable-hash)
                     0))
  (detect-duplicates (symbols-result-consts result))
  (match result
    [(symbols-result consts labels datas) (reverse (assemble instructions consts labels datas '()))]))

(provide primplify)
