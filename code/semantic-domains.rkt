#lang racket

;;; =================================
;;; Semantic Domains and Environments
;;; =================================

(provide
  expressible-value?
  denotable-value?
  proc
  prim-proc
  closure
  proc?
  prim-proc?
  closure?
  continuation
  continuation?
  )

;(require "env.rkt")
(require (only-in "env.rkt" env?))

;;; Expressible Values (types of values returned by
;;; evaluating an ast)

;;; ========================================

;;; expressible-value ::=
;;;    number | boolean | proc

;;; Denotable Values (types of values denoted by
;;; identifiers)
;;; ============================================

;;; denotable-value ::=  Ref(expressible-value)
;;;

;;; expressible-value? is the set of things that are the
;;; results of evaluation of expressions (asts).

(require eopl/eopl)
(require "ast.rkt")

;;; Procedure ADT
;;; ==============



(define-datatype proc proc? 
  [prim-proc
    ;; prim refers to a scheme procedure
    (prim procedure?)
    ;; sig is the signature
    (sig (list-of procedure?))] 
  [closure
    (formals (list-of symbol?))
    (body ast?)
    (env env?)]
  [continuation
    (k procedure?)]
 ; [rec-closure
  ;  (formals (list-of symbol?))
   ; (body ast?)]
  )




;;; Subtype Predicates
;;; ==================

;;; prim-proc? : proc? -> boolean?
(define prim-proc?
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #t]
      [else #f])))



;;; closure? : proc? -> boolean?
(define closure? 
  (lambda (p)
    (cases proc p
      [closure (formals body env) #t]
      [else #f])))

;;; continuation? : proc? -> boolean?
(define continuation? 
  (lambda (p)
    (cases proc p
      [continuation (k) #t]
      [else #f])))

;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (lambda(thing)
    (or(number? thing) 
      (boolean? thing)
      (proc? thing)
      )))


;;; denotable-value? :any/c -> boolean?
(define denotable-value? expressible-value?)



