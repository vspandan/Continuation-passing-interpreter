#lang racket

;;; =============================================================
;;; Evaluator for the STORE-PASSING/IMPLICIT-ALLOCATION  language
;;; =============================================================
  

(require eopl/eopl)
(require "ast.rkt")
(require "semantic-domains.rkt")
(require "store-list.rkt")
(require "env.rkt")
(require "primitives.rkt")
(require "init-store-env.rkt")
(require "parser.rkt")

(provide
   eval-ast/k
   top-k
   *resume*
   )


;;; answer? = store? expressible-value?

;;; eval-ast : [ast? env? store?]-> answer?
;;; eval-ast :  throws error

(define top-k
  (lambda([v "uncaught exception"])
    v))


(define eval-ast/k
  (lambda (a env k exn-k)
    (cases ast a
      [number (datum) (k datum)]
      [boolean (datum) (k datum)]
      [id-ref (sym) (lookup-env/k env sym k exn-k)]
      [ifte (test then else-ast)
         (eval-ast/k test env 
                     (lambda(t)
                       (if (boolean? t)
                           (eval-ast/k (if t then else-ast) env k exn-k)
                           (top-k (format "ifte test is not a boolean ~a" a)))) exn-k)
            ]
      [assume (binds body)
         (map-simple/k bind-id binds
                       (lambda(ids)
                         (map-simple/k bind-ast binds
                            (lambda(asts)
                              (eval-asts/k asts env
                                  (lambda(vals)
                                    (eval-ast/k body (extended-env ids vals env) k exn-k)) exn-k)))))]

      [recursive (fbinds fbody) 
          (map-simple/k fbind-id fbinds
                       (lambda(ids)
                         (map-simple/k fbind-formals fbinds
                           (lambda(formals)
                             (map-simple/k fbind-body fbinds
                                           (lambda(asts)
                                             (eval-ast/k fbody (extended-rec-env ids formals asts env) k exn-k)))))))]

      [function (formals body)
             (k (closure formals body env))] 
      [app (rator rands)
          (eval-ast/k rator env
                   (lambda(p)
                     (if (proc? p)
                         (eval-asts/k rands env 
                               (lambda(args)
                                 (apply-proc/k p args k exn-k)) exn-k)
                         (top-k (format "application rator is not a proc ~a" a))
                         )) exn-k)]
      
      [abort(abt)
            (eval-ast/k abt env top-k exn-k)]
      
      [break(bk)
            (eval-ast/k bk env 
                    (lambda(v)
                      (begin 
                        (set! resumek k)
                        (set! *global* v)
                        (top-k (format "breaking with value ~a" v))))
                        exn-k)]
      
      [try(body exn-id handler)
          (eval-ast/k body env k
                      (lambda(exc)
                        (let ([newenv (extended-env (list exn-id) (list exc) env)])
                          (eval-ast/k handler newenv k exn-k)))
                      )]
      
      [throw(exn-ast)
            (if (eq? exn-k top-k)
                 (top-k)
                 (eval-ast/k exn-ast env exn-k exn-k))]
      
      [letcc (id body)
         (let ([newenv (extended-env (list id) (list (continuation k)) env)])
           (eval-ast/k body newenv k exn-k))]
      
      [else (error 'eval-ast "unable to handle some ast cases")]
      
    )))

;;;global value
(define *global* -1)


;;;resume
(define *resume*
  (lambda([args -2])
    (if (eq? *global* -1)
        (top-k "nothing to resume")
        (if (eq? args -2)
            (let ([t *global*]) 
;               (set! *global* -1)
               (resumek t))
            (begin 
 ;              (set! *global* -1)
               (resumek args))))))
            
;;;resumek
(define resumek
  '())
  


                           
;;; eval-asts : [(listof ast?) env? continuation]

(define eval-asts/k
  (lambda (asts env k exn-k) 
    (if (empty? asts)
        (k '())
        (eval-ast/k (first asts) env 
                    (lambda(v)
                      (eval-asts/k (rest asts) env
                                   (lambda(w)
                                     (k (append (list v) w))) exn-k)) exn-k))
    ))

      
;;;map-simple/k

(define map-simple/k
  (lambda(f ls k)
    (if (null? ls)
        (k '())
        (map-simple/k f (rest ls) 
                      (lambda(v)
                        (k (cons (f (first ls)) v)))))))


;;; apply-proc :
;;;  [proc? (list-of expressible-value?) store?]
;;;    -> answer?

(define apply-proc/k 
  (lambda (p args k exn-k)
    (cases proc p
      [prim-proc(prim sig)
            (apply-prim-proc/k prim sig args k)]
      [closure(formals body env)
         (apply-closure/k formals body env args k exn-k)]
      
      [continuation(k1)
         (apply-continuation k1 args)] 
;      [rec-closure(formals body)
 ;        (apply-closure formals body env args)]
      )         
    ))


;;; apply-prim-proc :
;;;  [procedure? (listof procedure?)
;;;     (listof expressible-value?)] -> expressible-value?
;;;
;;; apply-prim-proc : throws error when number or type of
;;;     args do not match the signature of prim-proc

(define apply-prim-proc/k
  (lambda(prim sig args k)
    (cond
      [(and
         (not (null? args))
         (eq? (- (length sig) 1) (length args))
         (andmap match-arg-type (rest sig) args))
       (k (apply prim args))]
      [else (top-k (format "incorrect number or type of arguments to ~a" prim))])))

;;; match-arg-type : [procedure? any/c] -> boolean?
(define match-arg-type
  (lambda (arg-type val) 
    (arg-type val)
    ))


;;; apply-closure : [closure? (listof expressible-value?)]
;;;                  -> answer?

(define apply-closure/k
  (lambda(formals body env args k exn-k)
    (let ([e (extended-env formals args env)])
      (eval-ast/k body e k exn-k))))

                                      
;;;
(define apply-continuation
  (lambda(k args)
    (k (first args))))
                       
                     
;;; Unit testing
;;; ============
#|
(require rackunit)

(define-simple-check
  (check-eval-ast? ast env store expected label)
  (let-values ([(store val) (eval-ast ast env store)])
    (check-equal? val expected label)))


(define s1-e1
  (make-init-store-env '(x y z) '(1 2 3)))

(define s1 (first s1-e1))
(define e1 (second s1-e1))

(check-eval-ast? (number 5)  e1  s1 5 "eval-ast: n5 test")
(check-eval-ast? (boolean #t)  e1  s1 #t "eval-ast: bt test")

(check-eval-ast? (id-ref 'x)  e1  s1 1 "eval-ast: id1 test")
(check-eval-ast? (id-ref 'y) e1  s1 2 "eval-ast: y test")

(check-eval-ast?
  (assume (list (make-bind 'x (number 4)))
    (id-ref 'x))
  e1
  s1
  4
  "eval-ast: new-ref1")

(check-eval-ast?
  (assume (list (make-bind 'x  (number 4)))
    (assume (list (make-bind 'y  (id-ref 'x)))
      (id-ref 'y)))
  e1
  s1
  4
  "eval-ast: new-ref2")

(check-eval-ast?
  (assume (list (make-bind 'x  (number 4)))
    (assume (list (make-bind 'ignore (set 'x (number 7))))
      (id-ref 'x)))
  e1
  s1
  7
  "eval-ast: new-ref3")


(check-eval-ast?
  (assume (list (make-bind 'x  (number 4)))
    (seq
      (list 
        (set 'x (number 7))
        (id-ref 'x))))
  e1
  s1
  7
  "eval-ast: new-ref3")

;; trying to set a non-reference
    
    (check-exn exn?
      (lambda ()
        (eval-ast 
          (assume (list (make-bind 'x  (number 4)))
            (set (number 7) (boolean #f)))
          e1
          s1)
        "eval-ast: error-set"))


(check-exn exn?
  (lambda ()
    (eval-ast
      (assume-rec (list (make-bind 'x) (id-ref 'x))
        (id-ref 'x))
      e1
      s1
      "eval-ast: uninitialized")))
|#