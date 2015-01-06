(define (make-queue) 
  (cons '() '()))

(define (empty-queue? q)
  (null? (front-ptr q)))
	  
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))

;; Is this really a good way of doing it? 
;; Shouldn't front-ptr be replaced by front, and extract the element itself?
;; Ah, no, it shouldn't. That's the job of this function. front-ptr always exists.
;; front-queue itself can be replaced by a different implementation
(define (front-queue q)
  (if (empty-queue? q)
	  (error "Trying to extract front of an empty queue -- FRONT-QUEUE" q)
	  (car (front-ptr q))))

(define (set-front-ptr! q new-pair)
  (set-car! q new-pair))

(define (set-rear-ptr! q new-pair)
  (set-cdr! q new-pair))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
	(cond ((empty-queue? q)
		   (set-front-ptr! q new-pair)
		   (set-rear-ptr! q new-pair)
		   q)
		  (else 
		   (set-cdr! (rear-ptr q) new-pair)
		   (set-rear-ptr! q new-pair)
		   q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
		 (error "Trying to delete element from an empty queue -- DELETE-QUEUE" q))
		(else
		 (set-front-ptr! q (cdr (front-ptr q)))
		 q)))

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
	(let ((new-value (logical-not (get-signal input))))
	  (after-delay inverter-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! input invert)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
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

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
	(define (set-signal! new-value)
	  (if (= signal-value new-value)
		  'done
		  (begin (set! signal-value new-value)
				 (call-each action-procedures))))
	(define (accept-action-procedure! proc)
	  (set! action-procedures (cons proc action-procedures))
	  (proc)
	  'action-added)
	(define (dispatch m)
	  (cond ((eq? m 'get-signal) signal-value)
			((eq? m 'set-signal!) set-signal!)
			((eq? m 'add-action!) accept-action-procedure!)
			(else (error "Unknown operation -- WIRE" m))))
	dispatch))

(define (call-each procedures)
  (if (null? procedures)
	  'done
	  (begin
		((car procedures))
		(call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
				  action
				  the-agenda))

(define (propagate the-agenda)
  (if (empty-agenda? the-agenda)
	  'done
	  (let ((first-item (first-agenda-item the-agenda)))
		(first-item)
		(remove-first-agenda-item! the-agenda)
		(propagate the-agenda))))

(define (probe name wire)
  (add-action! wire
			   (lambda ()
				 (newline)
				 (display name)
				 (display " ")
				 (display (current-time the-agenda))
				 (display " New-value = ")
				 (display (get-signal wire)))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
	(or (null? segments)
		(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
	(let ((q (make-queue)))
	  (insert-queue! q action)
	  (make-time-segment time q)))
  (define (add-to-segments! segments)
	(if (= (segment-time (car segments)) time)
	  (insert-queue! (segment-queue (car segments))
					 action)
	  (let ((rest (cdr segments)))
		(if (belongs-before? rest)
		    (set-cdr! segments
					  (cons (make-new-time-segment time action)
							(cdr segments)))
			(add-to-segments! rest)))))
  (let ((segments (segments agenda)))
	(if (belongs-before? segments)
	  (set-segments! agenda
					 (cons (make-new-time-segment time action)
						   segments))
	  (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
	(delete-queue! q)
	(if (empty-queue? q)
	    (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
	  (error "Agenda is empty -- FIRST-AGENDA-ITEM")
	  (let ((first-seg (first-segment agenda)))
		(set-current-time! agenda (segment-time first-seg))
		(front-queue (segment-queue first-seg)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input1 (make-wire))
(define input2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

(half-adder input1 input2 sum carry)
(set-signal! input1 1)
(propagate the-agenda)
(set-signal! input2 1)
(propagate the-agenda)
