#lang racket

(provide load-primp run-primp)

(define MEM-SIZE 100)

(define mem (make-vector MEM-SIZE 0))
(define pc 0)
(define halted? #f)

;; primp-error: string value ... -> void
;; prints error message and other useful information

(define (primp-error . args)
  (apply printf args)
  (printf "PC: ~a" pc)
  (set! halted? #t)
  (error 'PRIMP "halted with error"))

;; valid-mem?: nat -> Boolean
;; checks for valid memory location

(define (valid-mem? i)
  (and (number? i) (>= i 0) (< i MEM-SIZE)))

;; mem-get: nat -> value
;; produces M[i], with error checking

(define (mem-get i)
  (cond
    [(valid-mem? i) (vector-ref mem i)]
    [else (primp-error "memory index out of range ~a\n" i)]))

;; mem-set!: nat value -> void
;; modifies M[i], with error checking

(define (mem-set! i newv)
  (cond
    [(valid-mem? i) (vector-set! mem i newv)]
    [else (primp-error "memory index out of range ~a\n" i)]))


;; load-primp: (listof list) -> void
;; initializes memory and PC, loads program into memory

(define (load-primp prog-list)
  (set! halted? #f)
  (set! pc 0)
  (vector-fill! mem 0)
  (for [(i MEM-SIZE)
        (c (in-list prog-list))]
    (mem-set! i c)))

;; run-primp: -> void
;; runs the PRIMP machine

(define (run-primp)
  (let loop ()
    (unless halted?
      (fetch-execute-once)
      (loop))))

;; fetch-execute-once: -> void
;; one step of fetch-execute cycle

(define (fetch-execute-once)
  (define inst (mem-get pc))
  (cond
    [(list? inst)
       (set! pc (add1 pc))
       (dispatch-inst inst)]
    [else
       (set! halted? #t)]))

;; dispatch-inst : S-exp primp -> void
;; Looks up inst in dispatch table and executes the
;; associated function.

(define (dispatch-inst inst)
  (when (empty? inst)
    (primp-error "Bad instruction: ~a\n" inst))
  (apply 
    (hash-ref dispatch-table 
              (first inst)
              (lambda () 
                (primp-error "Bad instruction: ~a\n" inst)))
         (rest inst)))

;; get-op-imm-or-mem: (union (list nat) nat Boolean) -> value
;; gets immediate operand or operand from memory location

(define (get-op-imm-or-mem op)
  (match op
    [(or (? number? v) (? boolean? v)) v] ; immediate 
    [x (get-op-mem op)])) ; rest

;; get-op-mem: (list nat) -> value
;; gets operand from memory location (indirect or indexed)

(define (get-op-mem op)
  (match op
    [`(,i) (mem-get i)] ; indirect
    [`(,i (,j)) (mem-get (+ i (mem-get j)))] ; indexed
    [x (primp-error "Bad operand: ~a\n" op)]))

;; set-dest!: (list nat) value -> void
;; sets new value of memory location

(define (set-dest! op v)
  (match op
    [`(,i) (mem-set! i v)] ; indirect
    [`(,i (,j)) (mem-set! (+ i (mem-get j)) v)] ; indexed
    [x (primp-error "Bad destination: ~a\n" op)]))

;; contracts for instructions: primp value ... -> void
;; side effects listed with each instruction

;; print-string : Print a string
(define (print-string s)
  (printf "~a" s))

;; print-val : Print an immediate value or contents of memory location
(define (print-val op)
  (define val (get-op-imm-or-mem op))
  (printf "~a" val))

;; bin-num-op : M[dest] <- src1 op src2
(define ((bin-num-op op) dest src1 src2)
  (define opnd1 (get-op-imm-or-mem src1))
  (define opnd2 (get-op-imm-or-mem src2))
  (unless (number? opnd1)
    (error 'PRIMP "First arithmetic operand not number: ~a ~a" opnd1 opnd2))
  (unless (number? opnd2)
    (error 'PRIMP "Second arithmetic operand not number. op:~a dest:~a opnd1:~a opnd2:~a" op dest opnd1 opnd2))
  (set-dest! dest (op opnd1 opnd2)))

;; add : M[dest] <- opnd1 + opnd2
(define add (bin-num-op +))

;; sub : M[dest] <- opnd1 - opnd2
(define sub (bin-num-op -))

;; mul : M[dest] <- opnd1 * opnd2
(define mul (bin-num-op *))

;; zero-source-error: (union (list nat) n string -> void

(define (zero-source-error src msg)
  (when (zero? (get-op-imm-or-mem src))
    (primp-error msg)))

;; div : M[dest] <- opnd1 / opnd2 [integer division]
(define (div dest src1 src2)
  (zero-source-error src2 "Divide by zero\n")
  ((bin-num-op quotient) dest src1 src2))

;; mod : M[dest] <- opnd1 mod opnd2
(define (mod dest src1 src2)
  (zero-source-error src2 "Modulus is zero\n")
  ((bin-num-op modulo) dest src1 src2))

;; equal : M[dest] <- opnd1 = opnd2
(define equal (bin-num-op equal?))

;; not-equal: M[dest] <- opnd1 <> opnd2
(define not-equal (bin-num-op (negate equal?)))

;; gt : M[dest] <- opnd1 = opnd2
(define gt (bin-num-op >))

;; ge : M[dest] <- opnd1 = opnd2
(define ge (bin-num-op >=))

;; lt : M[dest] <- opnd1 = opnd2
(define lt (bin-num-op <))

;; le : M[dest] <- opnd1 = opnd2
(define le (bin-num-op <=))

;; bin-logical-op : M[dest] <- opnd1 op opnd2
(define ((bin-logical-op op) dest src1 src2)
  (define opnd1 (get-op-imm-or-mem src1))
  (define opnd2 (get-op-imm-or-mem src2))
  (unless (boolean? opnd1)
    (primp-error "First logical operand not Boolean: ~a ~a\n" opnd1 opnd2))
  (unless (boolean? opnd2)
    (primp-error "Second logical operand not Boolean: ~a ~a\n" opnd1 opnd2))
  (set-dest! dest (op opnd1 opnd2)))

(define land (bin-logical-op (lambda (b1 b2) (and b1 b2))))

(define lor (bin-logical-op (lambda (b1 b2) (or b1 b2))))

(define (lnot dest src)
  (define opnd (get-op-imm-or-mem src))
  (unless (boolean? opnd)
    (primp-error "Logical operand not Boolean: ~a\n" opnd))
  (set-dest! dest (not opnd)))

;; move : M[dest] <- M[src]

(define (move dest src)
  (set-dest! dest (get-op-imm-or-mem src)))

;; jump: PC <- loc

(define (jump loc)
  (define tgt (get-op-imm-or-mem loc))
  (unless (valid-mem? tgt)
    (primp-error "Illegal jump target: ~a\n" tgt))
  (set! pc tgt))

;; jsr : M[dest] <- PC ; PC <- loc

(define (jsr dest loc)
  (define tgt (get-op-imm-or-mem loc))
  (unless (valid-mem? tgt)
    (primp-error "Illegal jsr target: ~a\n" tgt))
  (set-dest! dest pc)
  (set! pc tgt))

;; branch : PC <- loc if opnd

(define (branch opnd loc)
  (define tgt (get-op-imm-or-mem loc))  
  (unless (valid-mem? tgt)
    (primp-error "Illegal branch target: ~a\n" tgt))
  (define tested (get-op-imm-or-mem opnd))
  (unless (boolean? tested)
    (primp-error "Tested value in branch not Boolean: ~a\n" tested))
  (when tested (set! pc tgt)))

;; dispatch table 

(define dispatch-table
  (hash
    'print-val print-val
    'print-string print-string
    'add add
    'sub sub
    'mul mul
    'div div
    'mod mod
    'equal equal
    'not-equal not-equal
    'gt gt
    'ge ge
    'lt lt
    'le le
    'land land
    'lor lor
    'lnot lnot
    'move move
    'jump jump
    'jsr jsr
    'branch branch))
