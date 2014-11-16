#lang racket
(require "../code/top.rkt")
(require "../code/run.rkt")
(require "../code/ast.rkt")
(require "../code/parser.rkt")
(require "../code/init-store-env.rkt")
(require "../code/semantic-domains.rkt")
(require "../code/eval-ast.rkt")

(require rackunit)

(check-equal? (go '(assume ([fib (function (n) (* n n))]) 
             (assume ([fib (function (n) (ifte (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))])
                     (fib 3)))) 5 "checking function scope")

(check-equal?
  (go '(recursive ([f (n) (ifte (0? n) 1 (* n (f (- n 1))))])
         (f 3)))
  6
  "go-factorial")



(check-equal?
  (go
    '(recursive ([even? (n) (ifte (0? n) #t (odd? (- n 1)))]
                 [odd?  (n) (ifte (0? n) #f (even? (- n 1)))])
       (even? 3)))
  #f
  "go-even")

(check-equal?
  (go '(+ 2 (abort 5))) 5 "go-abort")


;;; error cases
(check-equal?
  (go '(+ 3 #t))
  (format "incorrect number or type of arguments to ~a" +))

(check-equal?
  (go '(/ 3 0))
  (format "incorrect number or type of arguments to ~a" /))

(check-equal?
  (go '(5 7))
  (format "application rator is not a proc ~a" (parse '(5 7))))



(check-equal? (go  '(+ 2  (break 3))) "breaking with value 3" "go-break-3")
(check-equal? (*resume*) 5 "go-resume")
(check-equal? (*resume* 4) 6 "go-resume-6")

(check-equal? (go  '(* (+ 2  (break 3)) (break 4))) "breaking with value 3" "go-break2-3")
(check-equal? (*resume*) "breaking with value 4" "go-resume2")
(check-equal? (*resume*) 20 "go-resume3")
(check-equal? (*resume* 8) 40 "go-resume-8")


(check-equal? (go '(throw 5)) "uncaught exception" "throw-uncaught")
(check-equal? (go '(+ 2 (try (* 3 4) v (+ v 7)))) 14 "try0")
(check-equal? (go '(+ 2 (try (+ 3 (throw 7)) v (+ v 4)))) 13 "try1")
(check-equal? (go '(+ 2 (try (+ 3 (throw (* 2 (throw 7)))) v (+ v
4)))) 13 "try2")
(check-equal? (go '(+ 2 (try (+ 3 (throw (* 2 (throw 7)))) v (+ v (throw 6)))))
              "uncaught exception" "try3")

(check-equal? (go '(letcc k (k 3))) 3 "letcc-1")
(check-equal? (go '(letcc k (+ 2 (k 3)))) 3 "letcc-2")
(check-equal? (go '(+ 2 (letcc k (* 3 (k 4))))) 6 "letcc-3")

