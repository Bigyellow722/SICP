(load "common.scm")

;; exercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3)))
	 )))

(f 3)

(define (f-iter count a b c)
  (cond ((= count 0) c)
	((= count 1) b)
	((= count 2) a)
	(else (f-iter (- count 1) (+ a (* 2 b) (* 3 c)) a b)
	      )))

(define (f-v2 n)
  (f-iter n 2 1 0))

(f-v2 4)


;; count change
(define (count-change-iter amount kinds-of-coins)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (count-change-iter (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
		 (count-change-iter amount (- kinds-of-coins 1))))))

(define (count-change amount)
  (count-change-iter amount 5))

(count-change-iter 100 5)
(count-change-iter 11 5)

(count-change 11)

;; exercise 1.12
(define (get-pascal-triangle x y)
  (cond ((or (= x 0) (= y 0)) 1)
	((= x y) 1)
	((> x y) (+ (get-pascal-triangle (- x 1) (- y 1))
		    (get-pascal-triangle (- x 1) y)))
	))

(get-pascal-triangle 5 3)

;; exercise 1.15
(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine (/ 3.14 6))

;; exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

(define (expt-v2 b counter)
  (expt-iter b counter 1))

;; fast exponentiation version
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; exercise 1.16
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter (square b) (/ n 2) a))
	(else (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt-v2 b n)
  (fast-expt-iter b n 1))


;;


;; exercis 1.20
