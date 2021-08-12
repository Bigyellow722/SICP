(load "common.scm")

;; exercise 1.3
;;
(define (larger-one-of-two a b)
  (if (> a b)
      a
      b))

(larger-one-of-two 2 5)

;;
(define (smaller-one-of-two a b)
  (if (> a b)
      b
      a))

;; 
(define (sum-of-largest-three-nums a b c)
  (if (> c (smaller-one-of-two a b))
      (+ c (larger-one-of-two a b))
      (+ a b)))

(sum-of-largest-three-nums 2 3 5)

;; exercise 1.5
;; infinite recursion
;;(define (p) (p))

;;(define (test x y)
;;  (if (= 0 x)
;;      0
;;      y))
;;
;; (test 0 (p))


;; 1.1.7
;; abs
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(abs -1)


;; sqrt
(define (sqrt y)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x)
		   x)))
  (sqrt-iter 1.0 y))

(define (sqrt-v2 y)
  (define (good-enough-v2? old new)
    (< (/ (abs (- old new)) old) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter-v2 guess y old-guess)
    (if (good-enough-v2? guess old-guess)
	guess
	(sqrt-iter-v2 (improve guess y) y guess)))
  (sqrt-iter-v2 1.0 y 2.0))

;;(sqrt 2)
(sqrt-v2 3)


;; exercise 1.1.8
(define (pow base exp)
  (if (= exp 0)
      1
      (* base (pow base (- exp 1)))
      ))

(pow 2 10)

(define (improve-cube y guess)
  (average guess (/ (+ (/ y (pow guess 2)) (* 2 guess)) 3)))

(improve-cube 8.0 2.0)

(define (extract-cube y)
  (define (improve-cube y guess)
    (average guess (/ (+ (/ y (pow guess 2)) (* 2 guess)) 3)))
  (define (good-enough-cube? old new)
    (< (/ (abs (- old new)) old) 0.0001))
  (define (extract-cube-iter guess y old-guess)
    (if (good-enough-cube? guess old-guess)
	guess
	(extract-cube-iter (improve-cube y guess) y guess)))
  (extract-cube-iter 1.0 y 2.0))

(extract-cube 10)
