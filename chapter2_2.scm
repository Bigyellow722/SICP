(load "common.scm")

(define no-more? null?)

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount (except-first-denomination coin-values))
		 (cc (- amount (first-denomination coin-values)) coin-values)))))


;; exercise 2.20
(define (same-parity first-term . rest-list)
  (let
      ((parity? (if (odd? first-term)
		   odd?
		   even?)))
    (define (same-parity-with-list ls)
      (if (null? ls)
	  '()
	  (if (parity? (car ls))
	      (cons (car ls) (same-parity-with-list (cdr ls)))
	      (same-parity-with-list (cdr ls)))))
  (if (null? rest-list)
      (list first-term)
      (cons first-term (same-parity-with-list rest-list)))))


(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(define (my-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (my-map proc (cdr items)))))

(define (scale-list-v2 items factor)
  (map (lambda (x) (* x factor)) iterms))


;;; exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-v2 items)
  (map (lambda (x) (* x x)) items))


(define (my-for-each proc items)
  (if (null? items)
      nil
      ((lambda (x) (proc (car x)) (my-for-each proc (cdr x))) items)))
