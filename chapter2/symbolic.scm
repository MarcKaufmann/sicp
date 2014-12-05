;; Exercise 2.54

(define (m-equal? a b)
  (cond ((and (null? a) (null? b)) #t)
		((or (null? a) (null? b)) #f)
		((and (symbol? a) (symbol? b))
		 (if (eq? a b)
		     #t
			 #f))
		((and (list? a) (list? b))
		 (if (m-equal? (car a) (car b))
		     (m-equal? (cdr a) (cdr b))
			 #f))
		(else #f)))

;; Exercise 2.55

(define (deriv expr var)
  (cond ((number? expr) 0)
		((variable? expr)
		 (if (same-variable? expr var) 1 0))
		((sum? expr)
		 (make-sum (deriv (addend expr) var)
				   (deriv (augend expr) var)))
		((product? expr)
		 (make-sum
		   (make-product (multiplier expr)
						 (deriv (multiplicand expr) var))
		   (make-product (deriv (multiplier expr) var)
						 (multiplicand expr))))
		((exponentiation? expr)
		 (let ((n (exponent expr))
			   (u (base expr)))
		   (make-product n (make-exponentiation u (- n 1)) (deriv u var))))
		(else
		  (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
		((=number? exponent 0) 1)
		((=number? exponent 1) base)
		(else (list '** base exponent))))

(define (same-variable? x y) 
  (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum . x)
  (let ((n (accumulate + 0 (filter number? x)))
		(symb (filter (lambda (i) (not (number? i))) x)))
	(cond ((and (=number? n 0) (null? symb)) 0)
		  ((=number? n 0) 
		   (if (null? (cdr symb)) 
			   (car symb) 
			   (append (list '+) symb)))
		  ((null? symb) n)
		  (else (append (list '+ n) symb)))))

(define (accumulate op id seq)
  (if (null? seq)
	  id
	  (op (car seq) (accumulate op id (cdr seq)))))


(define (make-product . x)
  (let ((n (accumulate * 1 (filter number? x)))
		(symb (filter (lambda (i) (not (number? i))) x)))
	(cond ((=number? n 0) 0)
		  ((and (=number? n 1) (null? symb)) 1)
		  ((=number? n 1) 
		   (if (null? (cdr symb)) 
			   (car symb) 
			   (append (list '*) symb)))
		  ((null? symb) n)
		  (else (append (list '* n) symb)))))

(define (make-sum-binary x y) 
  (cond ((=number? x 0) y)
		((=number? y 0) x)
		((and (number? x) (number? y)) (+ x y))
		(else (list '+ x y))))

(define (=number? a b)
  (and (number? a)
	   (number? b)
	   (= a b)))

(define (make-product-binary x y) 
  (cond ((=number? x 1) y)
		((=number? y 1) x)
		((and (number? x) (number? y)) (* x y))
		(else (list '* x y))))

(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))

(define (product? x)
  (and (pair? x) (eq? '* (car x))))

(define (addend s) (cadr s))

(define (augend s) 
  (let ((a (cddr s)))
	(cond ((null? a) (error "Not a sum " s))
		  ((null? (cdr a)) (car a))
		  (else (append (list '+) a)))))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (let ((a (cddr p)))
	(cond ((null? a) (error "Not a product " p))
		  ((null? (cdr a)) (car a))
		  (else (append (list '*) a)))))

