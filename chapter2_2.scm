(load "common.scm")
(load "mylist.scm")

(define no-more? null?)

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount (except-first-denomination coin-values))
		 (cc (- amount (first-denomination coin-values)) coin-values)))))


;; exercise 2.20
