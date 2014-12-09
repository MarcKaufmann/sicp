;; 2.5. Systems with Generic Operations
;;
;; I will only implement the generic addition, the rest is all equivalent boilerplate.

(define (add x y) (apply-generic 'add x y))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (contents datum)
  (if (pair? datum)
	  (cdr datum)
	  (error "Bad tagged datum -- CONTENTS" datum)))

(define (type-tag datum)
  (if (pair? datum)
	  (car datum)
	  (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (apply-generic proc . args)
  ;; Get tags of args; get procedure for this tag; apply it if it exists.
  (let ((type-tags (map type-tag args)))
	(let ((func (get proc type-tags)))
	  (if func
		  (apply f (map contents args))
		  (error "No such procedure for these types -- APPLY-GENERIC" (list proc type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; Now install the different number packages.

(define (install-scheme-number-package)
  (define (tag x)
	(attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
	   (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
	   (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
	   (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
	   (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
	   (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
	(let ((g (gcd n d)))
	  (cons (/ n g) (/ d g))))
  (define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))
  (define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
				 (* (numer y) (denom x)))
			  (* (denom x) (denom y))))
  (define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
			  (* (denom x) (denom y))))
  (define (div-rat x y)
	(make-rat (* (numer x) (denom y))
			  (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
	   (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
	   (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
	   (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
	   (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
	   (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

; Complex numbers

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
	((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
						 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
						 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
					   (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
					   (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
	   (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
	   (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
	   (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
	   (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
	   (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
	   (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Exercise 2.77.
;; This assumes that the rectangular and polar packages have been installed.
;; In that case, both packages have put a version of real-part, magnitude etc. in the table.
;; Then by doing (put 'real-part '(complex) real-part, those functions now call real-part, 
;; strip the argument of the type complex and check whether to call real-part for rectangular or polar. 
;; So apply-generic gets called twice, once for each tag.
;; Must be true, if the final operation is performed on type-less contents.
;; I am not sure that SICP is consistent in its use of '(complex) vs 'complex in the argument lists.

;; One way of doing it is as follows:
;; And ensure that the scheme-number package stops tagging its output.
;; Otherwise we need to change attach-tag.
;; But that seems the wrong place for that change.
;; Or maybe not? I'll implement it here for safety.

(define (attach-tag type-tag contents)
  (if (and (symbol? type-tag) (eq? type-tag 'scheme-number))
 	  contents
	  (cons type-tag contents)))

(define (contents datum)
  (cond ((number? datum) datum)
		((pair? datum) (cdr datum))
		(error "Bad tagged datum -- CONTENTS" datum)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
		((pair? datum) (car datum))
		(error "Bad tagged datum -- TYPE-TAG" datum)))

;; Exercise 2.79.

(define (equ? x y)
  (apply-generic 'equ? x y))

;; And then add all the 
;; (put 'equ? '(complex complex) equ?)
;; lines in all the packages (replace complex with rational, scheme-number as appropriate)
;; and where equ? is internally defined as the equality operation.

;; Exercise 2.80.

(define (=zero? x) (apply-generic '=zero? x))

;; And, as put in the right place the following:
;; (put '=zero? '(complex) (lambda (x) (and (zero? (real-part x)) (zero? (imag-part x)))))

;; Exercise 2.81.
;; a) Infinite recursion, since it will call (apply-generic 'exp x y) over and over after coercing it to itself.
;; b) Nope, he's wrong. If the function is defined for '(complex complex), then it will be found
;; before any coercing is tried. And if it is not found, then we don't want to coerce, due to problem in a).
;; c) Boring, wont' do.

;; Exercise 2.82.
;; What we really want is a minimal common type, or some such, i.e. a supertype that contains all types in the list.
;; This type may in general depend on the procedure, for numbers that's not the case though.
;; An example where trying out the types of the arguments in the procedure would not work is with polygons:
;; A triangle isn't a rectangle, but both are polygons.

;; Exercise 2.83.
;; Add the following code to the rational number package.

(define (raise-number->rational n)
  (cond ((not (number? n))
		 (error ("Argument is not an integer, need integer argument -- RAISE-NUMBER->RATIONAL" (list n))))
		(make-rat n 1)))

;; Exercise 2.84.
;;

(define (union-number u v)
  ;; Return the 'union' of u and v, where these are any of 'scheme-number, 'rational, 'complex
  (cond ((not (and (symbol? u) (symbol? v)))
		 (error ("Tags need to be of type symbol -- UNION-NUMBER" (list u v))))
		((or (eq? u 'complex) (eq? v 'complex))
		 ('complex))
		((or (eq? u 'rational) (eq? v 'rational))
		 ('rational))
		(else 'scheme-number)))

;; This would not work as is, as the procedures are not installed with just one type.
;; But that is the right way of installing them, with only 'complex (or '(complex)), not number of args.

(define number-tower '(scheme-number rational complex))

(define (raise-to-type n super-type)
  (define (raise-next n tower)
	(cond ((eq? (type-tag n) (super-type)) n)
		  ((not (pair? tower))
		    (error "Cannot raise number any further -- RAISE-NEXT" (list n tower)))
		  ((eq? (type-tag n) (car tower))
		    ;; This requires that raise has been installed as generic in previous example.
		    (raise-next (raise (car tower) (cadr tower) n) (cdr tower)))
		  (else
		    (raise-next n (cdr tower)))))
  (raise-next n number-tower))
			

(define (apply-generic op . args)
  (let ((super-type (accumulate union-number
							  '()
							  (map type-tag args))))
	(let ((proc (get op type-tag))
		  (raised-args (map (lambda (x) 
							  (raise-to-type x 'super-type))
							(map contents args))))
	  (if proc
		  (apply proc raised-args)
		  (error "No method for these types"
				 (list op type-tags))))))

;; TODO: If I feel like it, I can do the extended exercise at the end of the section.
