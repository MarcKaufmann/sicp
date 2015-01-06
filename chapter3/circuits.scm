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

(define mem-fib
  (memoize (lambda (n)
			 (cond ((= n 0) 0)
				   ((= n 1) 1)
				   (else (+ (mem-fib (- n 1))
							(mem-fib (- n 2))))))))

;; Exercise 3.28

(define (inverter input output)
  (define (invert)
	(let ((new-value (logical-not input)))
	  (after-delay inverter-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! input invert)
  'ok)

(define (logical-not s)
  (con ((= s 0) 1)
	   ((= s 1) 0)
	   (else (error "Invalid signal -- LOGICAL-NOT" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
	(let ((new-value 
			(logical-and (get-signal a1) (get-signal a2))))
	  (after-delay and-gate-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
	(let ((new-value 
			(logical-or (get-signal a1) (get-signal a2))))
	  (after-delay or-gate-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (signal? s)
  (or (= s 0) (= s 1)))

(define (logical-or s1 s2)
  (if (and (signal? s1) (signal? s2))
	  (cond ((or (= s1 1) (= s2 1)) 1)
			(else 0))
	  (error "Invalid signal -- LOGICAL-AND" (list s1 s2))))

(define (logical-and s1 s2)
  (if (and (signal? s1) (signal? s2))
	  (cond ((and (= s1 1) (= s2 1)) 1)
			(else 0))
	  (error "Invalid signal -- LOGICAL-AND" (list s1 s2))))

;; Exercise 3.29
;; Delay of or-gate is 2 * invert-delay + and-delay

(define (compound-or-gate o1 o2 output)
  (invert (and (invert o1) (invert o2) output)))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
	(or-gate a b d)
	(and-gate a b c)
	(inverter c e)
	(and-gate d e s)
	'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
		(c1 (make-wire))
		(c2 (make-wire)))
	(half-adder b c-in s c1)
	(half-adder a s sum c2)
	(or-gate c1 c2 c-out)
	'ok))

;; Exercise 3.30
;; Delay: n times the and-delay

(define (ripple-carry a_n b_n s_n c)
  (define (iter-ripple-carry a_k b_k s_k c-out)
	(if (null? a_k)
	  'ok
	  (let ((c-in (make-wire)))
		(full-adder (car a_n) (car b_n) c-in (car s_k) c-out)
		(iter-ripple-carry (cdr a_k b_k s_k c-in)))))
	(iter-ripple-carry a_n b_n s_n c))
