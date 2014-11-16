#lang racket

;;; Primitive Procedures
;;; --------------------


(provide
  +p -p *p /p <p <=p eq?p 0?p top-env lookup-env/k)

(require eopl/eopl)
(require "semantic-domains.ss")
(require "env.rkt")

(define nonzero? (and/c number? (not/c zero?)))


(define +p
  (prim-proc +
    (list number? number? number?)))

(define -p
  (prim-proc -
    (list number? number? number?)))

(define *p
  (prim-proc *
    (list number? number? number?)))

(define /p
  (prim-proc /
    (list number? number? nonzero?)))

(define <p
  (prim-proc  <
    (list boolean? number? number?)))

(define <=p
  (prim-proc   <=
    (list boolean? number? number?)))

(define eq?p
  (prim-proc eq?
    (list boolean? expressible-value? expressible-value?)))

(define 0?p
  (prim-proc zero?
    (list boolean? number?)))

(define top-env 
  (extended-env '(+  -  *  /  <  <=  eq?  0?) (list +p -p *p /p <p <=p eq?p 0?p) (empty-env))
  )


;;; Returns the loction of the element in a list, -1 if the
;;; element is absent.

;;; list-index : [(listof any/c)  any/c] -> 
(define list-index
  (lambda (ls a)
    (letrec ([loop
               (lambda (ls ans)
                 (cond
                   [(null? ls) -1]
                   [(eq? (first ls) a) ans]
                   [#t (loop (rest ls) (+ 1 ans))]))])
      (loop ls 0))))



;;; lookup-env: [env?  symbol?] -> any/c
;;; lookup-env: throws "unbound identifier" error
(define lookup-env/k
  (lambda (e x k exn-k)
    (cases env e
      [empty-env()
        (exn-k (format "unbound identifier ~a" x))]
      [extended-env (syms vals outer-env)
        (let ([j (list-index syms x)])
          (cond
            [(= j -1) (lookup-env/k outer-env x k exn-k)]
            [#t (k (list-ref vals j))]))]
      [extended-rec-env(fids fformals fbody outer-env)
        (let ([j (list-index fids x)])
          (cond
            [(= j -1) (lookup-env/k outer-env x k exn-k)]
            [#t 
             (let [(formal (list-ref fformals j)) (body (list-ref fbody j))]
               (k (closure formal body e))
               )]))]
      )))


