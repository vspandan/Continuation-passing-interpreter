#lang racket

;;; ================================
;;; Top-level setup for the language
;;; ================================

(require "run.rkt")
(require "parser.rkt")
(require "eval-ast.rkt")

(provide
  go)

(define go
  (lambda (e)
    (run (parse e))))

;;; Unit testing
;;; ============
(require rackunit)

#|
(check-equal?
  (go '(assume-rec ([f (function (n) (ifte (0? n) 1 (* n (f (- n 1)))))])
         (f 3)))
  6
  "go-factorial")



(check-equal?
  (go
    '(assume-rec ([even? (function (n) (ifte (0? n) #t (odd? (- n 1))))]
                  [odd?  (function (n) (ifte (0? n) #f (even? (- n 1))))])
       (even? 3)))
  #f
  "go-even")

(check-equal?
  (go
    '(assume  ([! (function (n)
                    (assume ([ans  1]
                             [i  n])
                      (assume-rec ([loop (function ()
                                           (ifte
                                             (eq? i 0)
                                             ans
                                             (seq
                                               (set ans (* ans i))
                                               (set i (- i 1))
                                               (loop))))])
                        (loop))))])
       (! 3)))
  6
  "go-factorial-imperative")





|#