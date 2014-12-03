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
