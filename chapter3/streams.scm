;; Exercise 3.58
;; (expand 1 2 3) ->  (0 (expand 2 2 3)) -> (0 1 (expand 1 2 3))
;; (expand 1 2 3) -> (1 (expand 1 2 3)) -> (1 1 1 1 ...)
;; It is the expansion of 1/2 in base 3.
;; In general, it is num/den in base radix.
;; This took me ages to convince myself of, after guessing it correctly immediately. Why? It seems obvious, and yet not.

;; Exercise 3.59

(define (invert-stream s)
  (cons-stream (/ 1 (stream-car s)) 
			   (invert-stream (stream-cdr s))))

(define (integrate-series s)
  (mul-streams s (invert-stream integers)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (let ((inverse-int (invert-stream integers)))
	(cons-stream 1 
			   (cons-stream 0
							(mul-streams cosine-series
										 (mul-streams (stream-cdr inverse-int)
													  (stream-cdr (stream-cdr inverse-int))))))))
;; Similar for sine

;; Easier, by define cosine from sine, and sine from cosine

(define cosine-series
  (cons-stream 1
			   (integrate-series (scale-stream sine-series (- 1)))))

(define sine-series
  (cons-stream 0
			   (integrate-series cosine-series)))

;; Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
			   (add-streams (mul-series s1 (stream-cdr s2))
							(mul-series (stream-cdr s1) s2))))

;; Exercise 3.61

(define (invert-unit-series s)
  (define inverse
	(cons-stream 1
				 (scale-stream (mul-series inverse (stream-cdr s)) (- 1))))
  inverse)

;; Exercise 3.62

(define (div-series s1 s2)
  (let ((constant-term (stream-car s2)))
	(if (= 0 constant-term)
	    (error "Naughty, you tried to divide by zero! -- DIV-SERIES")
		(begin
		  (scale (mul-series s1 
							 (invert-unit-series
							   (scale s2 s2 (/ 1 constant-term))))
				 (/ 1 constant-term))))))
