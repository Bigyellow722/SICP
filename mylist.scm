
;; list operation
;; address
(define (list-ref terms n)
  (if (= n 0)
      (car terms)
      (list-ref (cdr terms) (- n 1))))

;; length
(define (list-length terms)
  (define (list-length-iter terms count)
    (if (null? terms)
	count
	(list-length-iter (cdr terms) (+ count 1))))
  (list-length-iter terms 0))


;; append
(define (list-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (list-append (cdr list1) list2))))

;; remove


;; last entry
(define (last-pair terms)
  (list-ref terms (- (list-length terms) 1)))


(define nil '())
;; reverse
(define (list-reverse terms)
  (if (null? terms)
      terms
      (list-append (list-reverse (cdr terms))
		   (cons (car terms) nil))))

