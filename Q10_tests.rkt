#lang racket

(require "PRIMPLexec.rkt")
(require "PRIMPLifier.rkt")
(require "Q10.rkt")

; (define a-primpl
;   (compile-simpl
;    '(
;      (fun (f x y)
;           (vars [(i 5)]
;                 (return (+ (+ i x) y))))
;      (fun (main)
;           (vars [(a 10) (b 10)]
;                 (print (f a b))
;                 (return 0)))
;      ))
;   )
;;

(define a-primpl
  (compile-simpl
   (list
    '(fun (fib n)
          (vars [] (iif (<= n 2)
                        (return 1)
                        (skip))
                (return (+ (fib (- n 1)) (fib (- n 2))))))
    ))
  )

a-primpl

(define primpl (primplify a-primpl))

primpl

(load-primp primpl)
(run-primp)
