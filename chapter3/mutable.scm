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
;; Action: Implement the turtle rabbit algorithm, really nice idea (I did not think of it)

;; Implementation agnostic
;; Requires implementation-specific underneath

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
	  
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)

(define (print-queue q)
  (front-ptr q))

;; Exercise 3.22

(define (make-queue-dispatch)
  (let ((front-ptr '())
		(rear-ptr '()))
	;; Pure wrapper for readability
	(define empty-queue?
	  (lambda () (null? front-ptr)))
	(define (insert-queue! item)
	  (let ((new-pair (cons item '())))
		(cond ((empty-queue?)
			   (set! front-ptr new-pair)
			   (set! rear-ptr new-pair)
			   front-ptr)
			  (else
			   (set-cdr! rear-ptr new-pair)
			   (set! rear-ptr new-pair)
			   front-ptr))))
	(define delete-queue!
	  (lambda () 
		(if (empty-queue?)
		    (error "Trying to delete from an empty queue -- DELETE-QUEUE!" front-ptr)
			(begin
			  (set! front-ptr (cdr front-ptr))
			  front-ptr))))
	(define (dispatch m)
	  (cond ((eq? m 'front-queue) (car front-ptr))
			((eq? m 'insert-queue!) insert-queue!)
			((eq? m 'delete-queue!) (delete-queue!))
			((eq? m 'empty-queue?) (empty-queue?))
			((eq? m 'print-queue) front-ptr)
			(else (error "Unknown action -- QUEUE" m))))
	dispatch))

(define q1 (make-queue-dispatch))
((q1 'insert-queue!) 'a)
((q1 'insert-queue!) 'b)

;; Exercise 3.23.

;; Bad Design, I should have all the elements as two pairs with
;; - pointer to next
;; - pointer to previous
;; - front and rear are part of the list, but special, and pointed at by one pair
;; - inserting at beginning at end is merely about inserting between first and next first vs previous last and last.
;; - same for popping off.

(define (make-box item previous next)
  (let* ((item-pair (list item previous))
		 (next-pair (list item-pair next)))
	next-pair))

(define (next box) (cdr box))
(define (previous box) (cdar box))

(define (set-previous! box item)
  (set-cdr! (car box) item))
(define (set-next! box item)
  (set-cdr! box item))

(define box1 (make-box 'a '() '()))
(define box2 (make-box 'b box1 '()))
(define box3 (make-box 'c '() '()))
(set-next! box3 box1)

(define (make-dequeue)
  (let ((front (make-box '() '() '()))
		(rear (make-box '() '() '())))
	(set-next! front rear)
	(set-previous! rear front)
	(list front rear)))

(define (front-box dq) (car dq))
;; OK, so it is cadr in the next, not cdr.
;; BUG ALERT: The reason is that cdr does not give the second element of a list (obviously)
;; but the second element wrapped in a list.
;; That totally bit me.
(define (rear-box dq) (cadr dq))

(define (empty-dequeue? dq)
  (eq? (next (front-box dq)) (rear-box dq)))

(define (get-item box) (caar box))

(define (insert-after! box new-box)
  (if (null? (next box))
	  (error ("Trying to install after the end of the dequeue -- INSERT-AFTER!" box))
	  (begin
		(let ((next-box (next box)))
		  (set-next! box new-box)
		  (set-next! new-box next-box)
		  (set-previous! new-box box)
		  (set-previous! next-box new-box)))))

(define (print-dequeue dq)
  (define (accumulate-list box acc)
	(cond ((eq? (front-box dq) (previous box)) acc)
		  (else 
			(let ((new-acc (cons (get-item (previous box)) acc)))
			  (accumulate-list (previous box) new-acc)))))
  (if (empty-dequeue? dq)
	  '()
	  (accumulate-list (rear-box dq) '())))

(define (front-insert-dequeue! dq item)
  (let ((new-box (make-box item '() '())))
	(insert-after! (front-box dq) new-box))
  (print-dequeue dq))

(define (rear-insert-dequeue! dq item)
  (let ((new-box (make-box item '() '())))
	(insert-after! (previous (rear-box dq)) new-box))
  (print-dequeue dq))

(define dq (make-dequeue))
(front-insert-dequeue! dq 'a)
(rear-insert-dequeue! dq 'b)
(front-insert-dequeue! dq 'c)


(define (front-dequeue dq)
  (if (empty-dequeue? dq)
	  (error "Trying to get the front of an empty dequeue -- FRONT-DEQUEUE" dq)
	  (get-item (next (front-box dq)))))

(define (rear-dequeue dq)
  (if (empty-dequeue? dq)
	  (error "Trying to get the rear of an empty dequeue -- REAR-DEQUEUE" dq)
	  (get-item (previous (rear-box dq)))))

;; The user should not have access to delete-after! and insert-after!, but I'm not bothering with that for now.

(define (delete-after! dq box)
  (if (or (null? (next box)) (null? (next (next box))))
	  (error "Trying to delete after the end of the list -- DELETE-AFTER!" (list dq box))
	  (begin
		(set-next! box (next (next box)))
		(set-previous! (next box) box))))

(define (front-delete-dequeue! dq)
  (delete-after! dq (front-box dq))
  (print-dequeue dq))

(define (rear-delete-dequeue! dq)
  (delete-after! dq (previous (previous (rear-box dq))))
  (print-dequeue dq))

;; Keeping the following to show how crappy my code was at first.
;;
;(define (front-ptr dq) (car dq))
;(define (rear-ptr dq) (car dq))
;
;(define (empty-dequeue? dq)
;  (null? (front-ptr dq)))
;
;(define (front-dequeue dq) (car (front-ptr dq)))
;(define (rear-dequeue dq) (car (rear-ptr dq)))
;
;(define (make-box item previous next)
;  (let ((item-pair (cons item previous)))
;	(let ((next-pair (cons item-pair next)))
;	  next-pair)))
;
;(define (set-front-dequeue! element)
;  (set-car! dq element))
;
;(define (set-rear-dequeue! element)
;  (set-cdr! dq element))
;
;(define (set-previous! box element)
;  (set-cdr! (car box) element))
;
;(define (set-next! box element)
;  (set-cdr! box element))
;
;(define (front-insert-dequeue dq item)
;  (cond ((empty-dequeue? dq) 
;		 (let ((new-element (make-box item '() '())))
;		   ((set-front-dequeue! new-element)
;			(set-rear-dequeue! new-element)
;			dq)))
;		(else
;		  (let* ((current-first (front-ptr dq))
;				 (new-element (make-box item '() current-first)))
;			(set-previous! current-first new-element)
;			(set-front-dequeue! dq new-element)
;			dq))))
;
;(define (rear-insert-dequeue dq item)
;  (cond ((empty-dequeue? dq) 
;		 (let ((new-element (make-box item '() '())))
;		   ((set-front-dequeue! new-element)
;			(set-rear-dequeue! new-element)
;			dq)))
;		(else
;		  (let* ((current-last (front-ptr dq))
;				 (new-element (make-box item '() current-last)))
;			(set-next! current-last new-element)
;			(set-rear-dequeue! dq new-element)
;			dq))))

;; Table

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

;; Multi-dimensional tables
;; I don't like the way they do it.
;; If you search for something with key's key_1, key_2, ..., key_n, it is just like the one-dimensional case
;; applied recursively. Their way of doing it looks super non-modular and crappy.
;; Ah, that's exercise 3.25, more or less. So let's do it.
;; Reminder: in order to pass an arbitrary number of arguments, on does (define (bla x . z ) ... )
;; Then z is automatically turned into a list.

;; OK, I can't get the following to work.
;; Write out first on paper?
;; It should not be as hard as it is.
;; Split in small bits, test those as I go along.
;; Print out statements.
;; Write debug functions that help me debug.
;; Woah, it finally seems to work, but it's ugly.
;; Action: How can I make this nicer, where am I going wrong?

(define (make-table)
  (let ((local-table (list '*table*)))
	(define (assoc records key)
	  (cond  ((null? records) false)
			 ((not (list? records)) false)
			 ((equal? (caar records) key)
			  (car records))
			 (else
			   (assoc (cdr records) key))))
	(define (lookup keys)
	  (define (drill-down table remaining-keys)
		;(display "\n Calling drill-down with table and keys: \n")
		;(display table)
		;(display "\n")
		(if (null? remaining-keys)
		    table
			(let ((sub-table (assoc (cdr table) (car remaining-keys))))
			  ;(display "\n Entering. Subtable is: \n")
			  ;(display sub-table)
			  ;(display "\n")
			  (if sub-table
				 (drill-down sub-table (cdr remaining-keys))
				 false))))
	  (drill-down local-table keys))
	(define (insert-table! value keys)
;	  (display "\n")
;	  (display "Called insert-table! with value and keys:")
;	  (display (list value keys))
;	  (display "\n")
;	  (display "Local table is:")
;	  (display local-table)
;	  (display "\n")

	  (define (insert-down! table remaining-keys)
		;(display "\n")
		;(display "Table: ")
		;(display table)
		;(display "\n")
		;(display "Remaining keys: ")
		;(display remaining-keys)
		;(display "\n")
		(cond ((null? remaining-keys)
			   (set-cdr! table value)
			   table)
			  (else
				(let ((sub-table (assoc (cdr table) (car remaining-keys))))
				  (if sub-table
					  (insert-down! sub-table (cdr remaining-keys))
					  (let ((sub-table (cons (cons (car remaining-keys) '())
											 (if (list? (cdr table)) (cdr table) '()))))
						  ;(display "\n")
						  ;(display "Inner loop.\n")
						  ;(display "\n")
						  ;(display table)
						  ;(display "\n sub table is:")
						  ;(display sub-table)
						  ;(display "\n")
						  (set-cdr! table sub-table)
						  ;(display "\n")
						  ;(display table)
						  ;(display "Exiting inner loop.")
						  ;(display "\n")
						  (insert-down! (car sub-table) (cdr remaining-keys))))))))
	  (insert-down! local-table keys)
	  ;(display "\n")
	  ;(display "Local table at end of insert-table! is:")
	  ;(display local-table)
	  )
	(define (dispatch m)
	  (cond ((eq? m 'lookup) lookup)
			((eq? m 'insert-table!) insert-table!)
			((eq? m 'print) local-table)
			(else 
			  (error "Unknown message -- TABLE" m))))
	dispatch))

(define t (make-table))
((t 'insert-table!) 1 (list 'a))
((t 'insert-table!) 2 (list 'b))
((t 'insert-table!) 'second-level (list 'b 'a))
((t 'lookup) (list 'b 'a))
((t 'lookup) (list 'b 'c))
