#lang racket

;;; ========================================
;;; Run expression in init-env and new-store 
;;; ========================================

(provide
  run)

(require "ast.rkt")
(require "init-store-env.rkt")
(require "semantic-domains.rkt")
(require "eval-ast.rkt")
(require "primitives.rkt")
(require "parser.rkt")

;;; run: ast? -> expressible-value?
(define run
  (lambda (ast)
    (if (ast? ast)
        (eval-ast/k ast top-env top-k top-k)
        (eval-ast/k (parse ast) top-env top-k top-k)
        )))

;;; Unit testing
;;; ============

(require rackunit)

;; (run (id-ref '0?))
     
#|

(check-equal?
  (run
      (assume (list (make-bind 'a (number 5))
                (make-bind 'b (number 6)))
        (app (id-ref '+)
          (list (id-ref 'a) (id-ref 'b)))))
  11 "run: assume-test")

  
(check-equal?
 (run
  (function                 ; (function (x y z) (+ x (* y z)))
   '(x y z)
   (app (id-ref '+)
        (list (id-ref 'x)
              (app (id-ref '*)
                   (list (id-ref 'y) (id-ref 'z)))))))
 (closure '(x y z)
   (app (id-ref '+)
     (list (id-ref 'x)
       (app (id-ref '*)
         (list (id-ref 'y) (id-ref 'z)))))
   *init-env*)
 "run: function-test")


;;; (assume-rec ([even? (n) (if (0? n) #t (odd? (- n 1)))]
;;;             [odd?  (n) (if (0? n) #f (even? (- n 1)))])
;;;    (even? 3))

(check-equal?
 (run
  (assume-rec
   (list
    (make-bind 'even?
               (function  '(n)
                (ifte (app (id-ref '0?) (list (id-ref 'n)))
                      (boolean #t)
                      (app (id-ref 'odd?)
                           (list (app (id-ref '-)
                                   (list (id-ref 'n) (number 1))))))))

    (make-bind 'odd?
               (function '(n)
                (ifte (app (id-ref '0?) (list (id-ref 'n)))
                      (boolean #f)
                      (app (id-ref 'even?)
                           (list (app (id-ref '-) (list (id-ref 'n) (number 1)))))))))
   
   (app (id-ref 'even?) (list (number 3)))))
  #f
  "run-assume-rec-test")











|#