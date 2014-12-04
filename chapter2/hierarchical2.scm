;; Starting a new file, since some functions now have custom definitions 
;; that I want to get rid of. I want to use the in-built map, reverse, etc.

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

;; Exercise 2.37

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  '()
	  (cons (accumulate op init (map (lambda (x) (car x)) seqs))
			(accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence) 
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (matrix-*-matrix m1 m2)
  (map (lambda (row) (matrix-*-vector m1 row)) (transpose m2)))

(define (transpose m)
  (accumulate-n cons '() m))

;; Exercise 2.38
;; Commutatitivity

;; Exercise 2.39

(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

(define (flatmap proc seq)
  (accumulate append (list ) (map proc seq)))

(define (smallest-divisor n)
  (define (get-smallest n guess)
	(cond ((> (square guess) n) n)
		  ((= 0 (remainder n guess)) guess)
		  (else (get-smallest n (+ guess 2)))))
  (if (even? n) 
	  2
	  (get-smallest n 3)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum?
			   (flatmap 
				 (lambda (i)
				   (map (lambda (j) (list i j))
						(enumerate-interval 1 (- i 1))))
				 (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
	  (list '())
	  (flatmap (lambda (x) 
				 (map (lambda (p) (cons x p))
					  (permutations (remove x s))))
			   s)))

;; Exercise 2.40.

(define (unique-pairs n)
  (accumulate append
			  '()
			  (map (lambda (i)
					 (map (lambda (j) (list j i))
						  (enumerate-interval 1 (- i 1))))
				   (enumerate-interval 1 n))))

;; Using flatmap

(define (unique-pairs n)
  (flatmap (lambda (i) 
			 (map (lambda (j) (list j i))
				  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
	   (filter prime-sum? 
			   (unique-pairs n))))

;; Exercise 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
			 (flatmap (lambda (j)
						(map (lambda (k)
							   (list k j i))
							 (enumerate-interval 1 (- j 1))))
					  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))

(define (triples-sum-to n s)
  (filter (lambda (triple)
			(= s (accumulate + 0 triple)))
		  (unique-triples n)))

;; Exercise 2.42.

(define (adjoin-position new-row k rest-of-queens)
  (append (list (list new-row k)) rest-of-queens))

(define (safe? k positions)
  ;; positions is a list of all the queens where the kth queen needs to be checked against the others
  (define (safe-from-next? new next)
	;; Check that rows, columns and diagonals are different
	(if (or (= (car new) (car next))
			(= (cadr new) (cadr next))
			(= (abs (- (car new) (car next)))
			   (abs (- (cadr new) (cadr next)))))
	  #f
	  #t))
  (define (safe-remaining? new-queen remaining-queens)
	(cond ((null? remaining-queens) #t)
		  ((safe-from-next? new-queen (car remaining-queens))
		    (safe-remaining? new-queen (cdr remaining-queens)))
		  (else #f)))
  (safe-remaining? (car positions) (cdr positions)))

(define (queens board-size)
  (define empty-board (list ))
  (define (queen-cols k)
	(if (= k 0)
	  (list empty-board)
	  (filter
		(lambda (positions) (safe? k positions))
		(flatmap
		  (lambda (rest-of-queens)
			(map (lambda (new-row)
				   (adjoin-position new-row k rest-of-queens))
				 (enumerate-interval 1 board-size)))
		  (queen-cols (- k 1))))))
	(queen-cols board-size))
