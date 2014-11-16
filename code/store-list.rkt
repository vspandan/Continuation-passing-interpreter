#lang racket
;;; ==============================
;;; List implementation of Stores
;;; ==============================

;;; A store is implemented as a list.

(require racket/match)

(provide
  store?
  ref?
  new-store
  new-ref
  new-refs
  deref
  setref
  setrefs
  )

;;; nat? : any/c -> boolean?
(define nat?
  (lambda (n)
    (and (integer? n)
         (>= n 0))))

(define ref? nat?)

(define storable-value? any/c)


;;; store? : any/c -> boolean?
(define store?
  (lambda(ln)
    (cond
      [(list? ln) #t]
      [else #f])))


;;; new-store : () -> store?
(define new-store
  (lambda()
    '())
  )


;;; store-length : store? -> nat?
(define store-length
  (lambda(ls)
      (length ls))  
  )

;;; returns a new store with a new reference containing val
;;; new-ref: [store? storable-value?] -> [store? ref?]
(define new-ref
  (lambda (s v)
    (append (list (cons v s)) (list (store-length s)))
    ))



;;; returns a new store with new references containing vals 
;;; new-refs :
;;; [store? (listof storable?)] -> [store? (listof ref?)]
(define new-refs
  (lambda (s vals) 
    (letrec ([loop
              (lambda(s v r)
                (cond
                  [(empty? v) (append (list s) (list r))]
                  [ (let ([p (new-ref s (first v))])
                      (loop (first p) (rest v) (append r (list (second p)))))
                    ]))])
      (let ([f (loop s vals '())])
        (values (first f) (second f))
        )
      )
    ))



;;;  deref: [store?  ref?] -> storable-value?
;;;  deref: "throws address out of bounds" error
(define deref
  (lambda (s r)
    (cond 
      [(or
          (>= r (length s))
          (< r 0))
          (error 'deref "address out of bounds ~a" r)]
      [else (list-ref s (- (length s) 1 r))])
    ))


;;;  setref: [store?  ref? storable-value?] -> store?
;;;  setref: "throws address out of bounds" error if
;;;  reference  out of s's bounds.
(define setref
  (lambda (s r v) 
    (let ([n (store-length s)])
      (if (< r n)
          (replace s (- n r 1) v)
          (error 'setref "address out of bounds ~a" r)))
    ))


;;;  setrefs: [store?  (listof ref?) (listof storable-value?)] -> store?
;;;  setrefs: "throws address out of bounds" error if
;;;  rs are out of the store s's  bounds. 
(define setrefs
  (lambda (s rs vs)
    (letrec ([loop
              (lambda(s r v)
                (cond
                  [(empty? v) s]
                  [ (let ([p (setref s (first r) (first v))])
                      (loop p (rest r) (rest v) ))
                    ]))])
      (loop s rs vs)
      )
    ))




;;; replace
;;; -------
;;; construct a list like ls except that ls contains v at
;;; index i

;;; replace : ([ls : (listof any/c)]
;;;            [i : (</c (length ls))]
;;;            [v : any/c]) ->
;;;           (listof any/c)

(define replace
  (lambda (ls i v)
    (cond
      [(eq? i (- (length ls) 1)) (append (take ls i) (list v))]
      [else (append (take ls i) (list v) (list-tail ls (+ i 1)))])   
    ))



;;; unit Testing
;;; ============

;;; Racket's unit testing framework
(require rackunit)

;;; implementation tests
;;; --------------------

(define-simple-check (check-store? thing)
  (store? thing))

(check-store? (new-store))

(check-equal? (store-length (new-store)) 0 "test:store-length-empty-store")
(check-equal? (new-ref (new-store) 'a)  '((a) 0) "test:new-ref-empty-store")
(check-equal? (first (new-ref (new-store) 'a))  '(a) "test:new-ref^1")
(check-equal? (first (new-ref (first (new-ref (new-store) 'a)) 'b)) '(b a) "test:new-ref^2")
(check-equal? (first (new-ref (first (new-ref (first (new-ref (new-store) 'a)) 'b)) 'c))
              '(c b a) "test:new-ref^3")

(check-equal?
 (match-let ([(list s r) (new-ref (new-store) 4)])
    (new-ref s 8))
 '((8 4) 1))

(check-equal?
 (match-let ([(list s r1) (new-ref (new-store) 4)])
    (match-let ([(list s r2) (new-ref s 8)])
       (setref s r1 7)))
 '(8 7))


(check-equal?
  (match-let ([(list s r1) (new-ref (new-store) 4)])
    (match-let ([(list s r2) (new-ref s 8)])
       (setref s r2 15)))
  '(15 4))

(check-equal?
  (match-let ([(list s r1) (new-ref (new-store) 4)])
    (match-let ([(list s r2) (new-ref s 8)])
      (setref s r2 (+ (deref s r1) (deref s r2)))))
  '(12 4))


;;; API tests
;;; ---------
(define s0 (new-store))
(define ans (new-ref s0 'a))
(define s1 (match-let ([(list s _) ans]) s))
(define r1 (match-let ([(list _ r) ans]) r))

(check-equal? r1 0)
(check-equal? (deref s1 0) 'a)
(check-exn exn? (lambda ()
                  (deref s1 1)))

(define s2 (setref s1 0 'b))
(check-equal? (deref s2 0) 'b)
(check-exn exn? (lambda () (setref s2 3 'c)))


(check-equal?
  (match-let ([(list s r) (new-ref (new-store) 4)])
    (let ([s (setref s r 7)])
      (deref s r)))
  7)


(check-equal?
  (match-let ([(list s r) (new-ref (new-store) 4)])
    (match-let ([(list s r) (new-ref s 8)])
      (deref s 0)))
  4)

(check-equal?
  (match-let ([(list s r) (new-ref (new-store) 4)])
    (match-let ([(list s r) (new-ref s 8)])
      (deref s r)))
  8)


