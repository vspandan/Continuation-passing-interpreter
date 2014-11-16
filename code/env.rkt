#lang racket

;;; =================================
;;; Semantic Domains and Environments
;;; =================================

(provide
  env
  env?
  empty-env
  extended-env
  extended-rec-env
  nat?
  )


;;; Expressible Values (types of values returned by
;;; evaluating an ast)


(require eopl/eopl)
(require "ast.rkt")

(define nat?
  (lambda (n)
    (and (integer? n)
         (>= n 0))))


(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of any/c))
    (outer-env env?)]
  [extended-rec-env 
    (fids (list-of symbol?))
    (fformals (list-of (list-of symbol?)))
    (fbody (list-of ast?))
    (outer-env env?)]
  )

;;; Subtype Predicates
;;; ==================

;;; empty-env? : env? -> boolean?
(define empty-env?
  (lambda (e)
    (cases env e
      [empty-env () #t]
      [else #f])))

;;; extended-env? : env? -> boolean?
(define extended-env?
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) #t]
      [else #f])))

;;; extended-rec-env? : env? -> boolean?
(define extended-rec-env?
  (lambda (e)
    (cases env e
      [extended-rec-env (fids fformals fbody outer-env) #t]
      [else #f])))




#|
;;; Unit testing
;;; ============
(require rackunit)

(check-pred env? (empty-env) "env?-empty-env")
(check-pred empty-env? (empty-env) "empty-env?-empty-env")
(check-exn exn? (lambda () (lookup-env (empty-env) 'a)) "lookup-empty-env-a")

(define e1
  (extended-env '(x y z) '(1 2 3) (empty-env)))

(check-pred env?  e1 "env?-extended-env")
(check-pred extended-env? e1 "extended-env?-extended-env")

(check-equal? 1 (lookup-env e1 'x) "lookup-e1-x")
(check-equal? 2 (lookup-env e1 'y) "lookup-e1-y")
(check-exn exn? (lambda () (lookup-env e1 'a)) "lookup-e1-a")

(define e2
  (extended-env '(w x) '(5 6) e1))

(check-equal? 5 (lookup-env e2 'w) "lookup-e2-w")
(check-equal? 6 (lookup-env e2 'x) "lookup-e2-x")
(check-equal? 2 (lookup-env e2 'y) "lookup-e2-y")
(check-equal? 3 (lookup-env e2 'z) "lookup-e2-z")
(check-exn exn? (lambda () (lookup-env e2 'a)) "lookup-e2-a")


|#

