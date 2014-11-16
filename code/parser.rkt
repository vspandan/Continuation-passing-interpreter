#lang racket

;;; =================================
;;; Parser for the RECURSION language
;;; =================================

;;; Concrete Syntax

;;; <exp> :=  <num> |
;;;           <bool>
;;;           <id> |
;;;           (ifte <exp> <exp> <exp>) |
;;;           (function (<id> ...) <exp) |
;;;           (assume ([<id> <exp>] ...) <exp>) |
;;;           (assume-rec ([<id> <exp>] ...) <exp>) |
;;;           (<exp> <exp> ...) |
;;;           (abort <exp>) |
;;;           (break <exp>) |
;;;           (try <exp> <id> <exp>) |
;;;           (throw <exp>) | 
;;;           (letcc <id> <exp>)


;;; example concrete syntax

;;; (assume-rec ([even? (function (n) (if (0? n) #t (odd? (sub1 n))))]
;;;             [odd?  (function (n) (if (0? n) #f (even? (sub1 n))))])
;;;    (even? 5))

(require racket/match)

(require "ast.rkt")

(provide
  id?
  parse)

(define *keywords*
  '(ifte function assume assume-rec seq set))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))
         

(define parse
  (lambda (d)
    (match d
     [(? number? n) (number n)]
     [(? boolean? b) (boolean b)]
     [(? id? x) (id-ref x)]
     [(list 'ifte a b c)  (ifte (parse a) (parse b) (parse c))]

     [(list
       'function
       (list (? id? x) ...)
       body)
      (function x (parse body))]
     
     [(list 'assume
        (list (list (? id? x) e) ...) body)
      (let* ([a (map parse e)]
             [b (map make-bind x a)])
        (assume b (parse body)))]
     [(list 'recursive
        (list (list (? id? x) formals e) ...) body)
      (recursive (map (lambda (fb-id fb-formal fb-ast) (make-fbind fb-id fb-formal (parse fb-ast))) x formals e) (parse body))  
      ]
     [(list 'abort e)
      (abort (parse e))]
     
     [(list 'break e)
      (break (parse e))]

     [(list 'try a (? id? x) b)
      (try (parse a) x (parse b))]

     [(list 'throw e)
      (throw (parse e))]

     [(list 'letcc (? id? x) e)
      (letcc x (parse e))]
     
     [(list rator rands ...)
      (let* ([rator (parse rator)]
             [rands (map parse rands)])
        (app rator rands))]
      
     [_ (error 'parse "don't know how to parse ~a" d)])))



;;; Unit Testing
;;; ============
(require rackunit)


(check-equal? (parse 4) (number 4) "parse-number")
(check-equal? (parse #t) (boolean #t) "parse-boolean")
(check-equal? (parse 'x) (id-ref 'x) "parse-id")

(check-equal?
 (parse '(ifte 3 4 8))
 (ifte (number 3) (number 4) (number 8))
 "parse-ifte")


(check-equal?
 (parse '(function (x y) 4))
 (function '(x y) (number 4))
 "parse-function")


(check-equal?
  (parse '(assume ([x 3]) 6))
  (assume (list (make-bind 'x (number 3))) (number 6))
  "parse-assume")

#|
(check-equal?
  (parse '(assume-rec ([f (function (x y) x)]
                       [g (function (m n) 5)])
            9))
  (assume-rec
    (list
      (make-bind 'f (function '(x y) (id-ref 'x)))
      (make-bind 'g (function '(m n) (number 5))))
    (number 9))
  "parse-assume-rec")


(check-equal?
  (parse '(assume-rec () 9))
  (assume-rec
    (list)
    (number 9))
  "parse-empty-assume-rec")
|#
(check-equal?
  (parse '(x y))
  (app (id-ref 'x)
       (list (id-ref 'y)))
  "parse-app")



(check-exn exn? (lambda () (parse "hello")) "parse-string-error")
(check-exn exn? (lambda () (parse '#(1 2))) "parse-vector-error")
(check-exn exn? (lambda () (parse '(1 . 2)) "parse-cons-error"))
(check-exn exn? (lambda () (parse '()) "parse-empty-error"))
(check-exn exn? (lambda () (parse '(set 4 5))) "parse-set-error")


