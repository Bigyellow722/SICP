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

(count-change-iter 100 5)
(count-change-iter 11 5)

;; exercise 1.12
(define (get-pascal-triangle x y)
  (cond ((or (= x 0) (= y 0)) 1)
	((= x y) 1)
	((> x y) (+ (get-pascal-triangle (- x 1) (- y 1))
		    (get-pascal-triangle (- x 1) y)))
	))

(get-pascal-triangle 5 3)
