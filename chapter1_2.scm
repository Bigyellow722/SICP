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


;; exercise 1.12
