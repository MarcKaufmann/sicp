;; Exercise 3.1.

(define (make-accumulator value)
  (define (accumulator addition)
	(if (number? addition)
	    (begin (set! value (+ value addition))
			   value)
	    (error "Can only accumulate numbers -- ACCUMULATOR" addition)))
  accumulator)
