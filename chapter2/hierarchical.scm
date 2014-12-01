;;; Hierarchical Data and Closure Property

;; Exercise 2.17.

(define (last-pair l)
  ;; No idea what I should output if l is empty. Empty list or error?
  (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))

;; Exercise 2.18.
;; I couldn't figure this one out without using append, or rewriting it.
;; Why is that? Is there a way to do this smarter?

(define (reverse l)
  ;; Again, not dealing with the empty list
  (if (null? (cdr l))
      (list (car l))
      (append (reverse (cdr l)) 
              (list (car l)))))
;; Yes, I can do better, by checking whether cdr l is empty or null, or a singleton or something like that.

;; Exercise 2.19.

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coin-values) (car coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

(define (no-more? coin-values) (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else 
		  (+ (cc amount
				 (except-first-denomination coin-values))
			 (cc (- amount
					(first-denomination coin-values))
				 coin-values)))))

;; Exercise 2.20.

;; Not the prettiest way of doing it, I'm sure.
(define (same-parity a . l)
  (define (get-same-parity l)
    (if (null? l)
	    l
        (if (even? (+ a (car l)))
            (cons (car l) (get-same-parity (cdr l)))
            (get-same-parity (cdr l)))))
  (cons a (get-same-parity l)))

(define (scale-list items factor)
  (if (null? items)
	  nil
	  (cons (* factor (car items))
			(scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
	  nil
	  (cons (proc (car items))
			(map proc (cdr items)))))

;; Exercise 2.21.

(define (square-list items)
  (if (null? items)
	  nil
	  (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.22
;; That's what I managed to get wrong when trying to reverse the list. 
;; cons requires an element, not a list, as it's first argument.

;; Exercise 2.23

(define (for-each proc items)
  (cond ((null? items) #t) 
		(else (proc (car items)) 
			  (for-each proc (cdr items)))))
