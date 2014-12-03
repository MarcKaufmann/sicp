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
;; Commented out as I later define append in terms of reverse, and reverse in terms of append, so this will fail.

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
	  '()
	  (cons (* factor (car items))
			(scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
	  '()
	  (cons (proc (car items))
			(map proc (cdr items)))))

;; Exercise 2.21.

(define (square-list items)
  (if (null? items)
	  '()
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

(define (count-leaves tree)
  (cond ((null? tree) 0)
		((not (pair? tree)) 1)
		(else (+ (count-leaves (car tree))
				 (count-leaves (cdr tree))))))

;; Exercise 2.25
;; Write the sequence of cdr's and car's that gives 7.
;; (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))). I forgot a car at the beginning.
;; Remember: cdr always returns a list, car returns an item.
;; (car (car (list (list 7))))
;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))
;; Totally screwed that one up. Did not realise that this is a list with two elements, so I have to get the second element by car'ing the cdr to get the nested list of two elements.

;; Exercise 2.27 (Deep-reverse)
;; (1 2) -> (2 1)
;; ((1) 2) -> (2 (1))
;; ((1 2) 3) -> (3 (1 2))
;; () -> ()
;; (1) -> (1); ((1 2)) -> ((2 1)); a list with a single element becomes (list (deep-reverse l))

;; God, this took ages. Again, the problem was/is that I get confused by what is a list and what is not.
;; I returned things as elements, rather than lists.

(define (deep-reverse l)
  (cond ((or (not (list? l)) (null? l)) l)
		(else
		  (append (deep-reverse (cdr l))
				  (list (deep-reverse (car l)))))))

;; What to do without append?
;; Probably requires something iterative to keep track of things.
;; This actually works!? Must be tons faster than the other version.

(define (deep-reverse2 l)
  (define (iter l answer)
	(cond ((null? l) answer)
		  ((null? (cdr l)) 
		   (cons (deep-reverse2 (car l) answer)))
		  (else 
			(iter (cdr l) (cons (deep-reverse2 (car l)) answer)))))
  (iter l ()))

;; Exercise 2.28

(define (fringe t)
  (cond ((null? t) t)
		((not (pair? t)) (list t))
		(else (append (fringe (car t)) 
					  (fringe (cdr t))))))

(define (scale-tree tree factor)
  (cond ((null? t) '())
		((not (pair? tree)) (* factor tree))
		(else (cons (scale-tree (car tree) factor)
					(scale-tree (cdr tree) factor)))))

;; Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (square tree))
		(else (cons (square-tree (car tree))
					(square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		     (square-tree-map sub-tree)
			 (square sub-tree)))
	   tree))

;; Exercise 2.31

(define (tree-map proc tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (proc tree))
		(else (cons (tree-map proc (car tree))
					(tree-map proc (cdr tree))))))

;; Exercise 2.32

(define (subsets s)
  (if (null? s)
	  (list '())
	  (let ((rest (subsets (cdr s))))
		(append rest 
				(map (lambda (element) (cons (car s) element))
					 rest)))))
;; Sequence operations

(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

;; Why would this not necessarily work?
;; It works for addition and multiplication
;; Is it assuming something about associativity? I think so.
;; E.g. the desired version gives (cons 1 (cons 2 (cons ...)))
;; whereas mine gives (... (cons 2 (cons 1 '()))...) which is different.
;; Commutatitivity is what I am thus assuming. Oh well.

(define (accumulate-own op initial sequence)
  (if (null? sequence) 
	  initial
	  (accumulate-own op (op (car sequence) initial) (cdr sequence))))

(define (accumulate op initial sequence)
  (if (null? sequence) 
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
			  0
			  (map square
				   (filter odd?
						   (enumerate-tree tree)))))

;; Exercise 2.33.

(define (map p sequence)
  (accumulate (lambda (x y)
				(cons (p x) y))
			  '()
			  sequence))

(define (append seq1 seq2)
  (accumulate cons 
			  seq2
			  seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1))
			  0 
			  sequence))

;; Exercise 2.34.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (a b) 
				(+ (* b x) a))
			  0
			  coefficient-sequence))

;; Exercise 2.35

(define (count-leaves tree)
  (accumulate +
			  0
			  (map (lambda (x) 1)
				   (enumerate-tree tree))))

;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  '()
	  (cons (accumulate op init (map (lambda (x) (car x)) seqs))
			(accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

