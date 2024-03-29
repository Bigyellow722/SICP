;; operate number
;; abs
(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; judge whether a number is a even number
(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))
;; square
(define (square x)
  (* x x))

;; average
(define (average y x)
  (/ (+ x y) 2))

;; gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; check prime
(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;; utils
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
