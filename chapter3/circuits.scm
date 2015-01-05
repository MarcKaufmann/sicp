(define (make-table)
  (list '*table*))

;; return the value associated with the key
(define (lookup key table) 
  (let ((record (associated key (cdr table))))
	(if record
	    (cdr record)
		false)))

;; return the key-value pair/record, where records is the table minus its extra header
(define (associated key records)
  (cond ((null? records) false)
		((equal? key (caar records)) (car records))
		(else (associated key (cdr records)))))

;; Overwrite existing value, if it exists
(define (insert! key value table) 
  (let ((record (associated key (cdr table))))
	(if record 
	    (set-cdr! record value) value)
	    (set-cdr! table 
				  (cons (cons key value) (cdr table))))
  'ok)


;; Exercise 3.27
;;
;; Memoization

(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
	(lambda (x)
	  (display table)
	  (let ((previously-computed-result (lookup x table)))
		(or previously-computed-result
			(let ((result (f x)))
			  (insert! x result table)
			  result))))))
