(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; Exercise 3.14
;; mystery reverses the list and mutates it

(define (mystery x)
  (define (loop x y)
	(if (null? x)
	    y
		(let ((temp (cdr x)))
		  (set-cdr! x y)
		  (loop temp x))))
  (loop x '()))

;; Exercise 3.16
;; If by number of pairs they mean different pairs as indicated by pointers at them, that's easy.
;; A cycle gives infinity and so never stops. 
;; A set of three different pairs gives three different pairs.
;; Counting of 4 is achieved by letting the second pair in the row (cadr) point to the third element.
;; 7 is achieved by letting the car point at the second, which points at the third. 
;; 1 (for the last) + 2 (for the second, which double counts the last) + 4 (for the first, which counts the second once and the third twice). 

;; Exercise 3.17

(define (is-in-list? x l)
  (cond ((null? l) #f)
		((eq? x (car l)) #t)
		(else (is-in-list? x (cdr l)))))

(define (count-pairs x)
  (define (iter-count-pairs x list-pairs found)
	(cond ((not (pair? x)) found)
		  ((is-in-list? x list-pairs) found)
		  (else (iter-count-pairs (cdr x) (cons x list-pairs) (+ found 1)))))
  (iter-count-pairs x '() 0))

(define (count-pairs-wrong x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs-wrong (car x))
		 (count-pairs-wrong (cdr x))
		 1)))

;; Exercise 3.18

(define (has-cycle? x)
  (define (iter-cycle x pairs-found)
	  (cond ((not (pair? x)) #f)
			((is-in-list? (cdr x) pairs-found) #t)
			(else (iter-cycle (cdr x) (cons (cdr x) pairs-found)))))
  (iter-cycle x (cons x '() ) ))

;; Exercise 3.19


