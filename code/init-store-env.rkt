#lang racket

#|

(provide
  make-init-store-env
  *init-store-env*
  *init-env*
  *init-store*)

;;; ===================
;;; Initial Environment
;;; ===================
(require "primitives.rkt")
(require "store-list.rkt")
(require "env.rkt")

;;; [(listof id?) (listof storable-value?) -> (list store? env?)
(define make-init-store-env
  (lambda (ids vals)
    (let-values ([(store refs) (new-refs (new-store) vals)])
      (let ([env (extended-env
                   ids refs (empty-env))])
        (list store env)))))


;;; (list store? env?)
(define *init-store-env*
  (make-init-store-env
       '(+  -  *  /  <  <=  eq?  0?)
   (list +p -p *p /p <p <=p eq?p 0?p)))

(define *init-store* (first *init-store-env*))
(define *init-env* (second *init-store-env*))

|#