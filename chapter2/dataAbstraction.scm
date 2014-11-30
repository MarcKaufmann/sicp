(define (gcd a b)
  (cond ((= a 0) b)
        ((= b 0) a)
        (else (gcd b (remainder a b)))))

;; This sign function does not take care of zero, it assumes it's positive.

(define (sign a)
  (if (negative? a)
      (- 1)
      1))

(define (make-rat n d) 
  (let ((g (gcd n d))
        (s (* (sign n) (sign d))))
    (let ((n (abs (/ n g)))
          (d (abs (/ d g))))
          (cons (* s n) d))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))))

(define (neg-rat x)
  (make-rat (* (- 1) (numer x)) (denom x)))

(define (inv-rat x)
  (make-rat (denom x) (numer x)))

(define (sub-rat x y)
  (add-rat x (neg-rat y)))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (mult-rat x (inv-rat y)))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;; Exercise 2.2.

(define (make-point x y)
  (cons x y))

(define (x-point x) (car x))

(define (y-point x) (cdr x))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((s (start-segment segment))
        (e (end-segment segment)))
    (make-segment (average (x-point s) (x-point e))
                  (average (y-point s) (y-point e)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3.
;; I assume that all rectangles have sides that are vertical or horizontal, i.e. parallel to the x- and y-axis.
;; Otherwise I'd need to introduce an inner product or some other notion of orthogonality.

; Use the left bottom and right up corner to defie a rectangle.

;; Exercise 2.4.

(define (cdr-2.4 z)
  (z (lambda (p q) q)))

;; Exercise 2.5.

(define (^ n)
  (define (power x k)
    (if (= k 0)
        1
        ((* x (power x (- k 1))))))
  (lambda (x) (power x n)))

(define (cons-int a b)
  (cons ((^ a) 2) ((^ b) 3)))

(define (power-divides divisor n power)
  (if (divides? divisor n)
      (power-divides (/ n divisor) (inc power))
      power))

(define (car-int z)
  (power-divides 2 z 0))

(define (cdr-int z)
  (power-divides 3 z 0))

;; Exercise 2.6.

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; zero maps all functions to the identity function
;; (add-1 zero) -> (lambda (f) (lambda (x) (f ((lambda (x) x) x)))) ->  (lambda (f) (lambda (x) (f x)))
;; one maps all functions to themselves 
;; two -> (lambda (f) (lambda (x) (f ((one f) x)))) -> (lambda (f) (lambda (x) (f (f x))))
;; two maps all functions to the function composed with itself
;; By induction, n is the function that maps f to f^n (f composed n times with itself).
;; (+ n m): + is the composition map. Why? n is the map that maps to f^n, m to f^m. 
;; (+ n m) maps to f^(n + m), the composition of n and m (so to speak).

;; Section 2.1.4.

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (inv-interval x)
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))

(define (div-interval x y)
  (mul-interval x (inv-interval y)))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

;; Exercise 2.8.

(define (neg-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

(define (sub-interval x y)
  (add-interval x (neg-interval y)))

;; Exercise 2.9.
;; Sum is easy to see.
;; Multiplication: multiply [-1,1] with [-1,0] and [-1,1]. Both give [-1,1], but have different widths.


;; Exercise 2.10.

(define (inv-interval x)
  (let ((u (upper-bound x))
        (l (lower-bound x)))
    (if (= u l)
        (display "error: division by zero-length interval")
        (make-interval (/ 1.0 u) (/ 1.0 l)))))

;; Problem with this is that raising the error does not stop the program execution.
;; Whatever, I don't care. I don't know how scheme handles these things.

(define (div-interval x y)
  (mul-interval x (inv-interval y)))

;; Exercise 2.11.
;; If all are positive, take product of mins and maxs.
;; If all are negative, the same but the max is product of mins and vice versa.
;; If only one lower end is negative, then the min is that one with the largest of the upper-bounds, and max is obvious.
;; If only one upper is positive, then upper bound is the product of the two lower bounds, and lower bound is pos with neg
;; If two are negative, we need to check things.
;; I'm not going to write this out.

;; Exercise 2.12.

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (lower-bound i) (upper-bound i)) 2))

(define (get-percent perc x)
  (/ (* perc x) 100.0))

(define (make-center-percent c p)
  (let ((w (get-percent p c)))
    (make-center-width c w)))

(define (percent i)
  (let ((c (center i))
        (w (width i)))
    (* 100.0 (/ w c))))

;; Exercise 2.14.

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Exercise 2.15.
;; She is right it gives tighter bounds. 
;; The reason is that par1 is the same as par2, except that it contains 'gratuituous' R1/R1.
;; This should not have any error (it is one, no matter what R1 is).
;; But the division algorithm treats it as if the R1 in numerator and denominator were independent.
;; So it adds noise.

;; Exercise 2.16.
;; Let's assume that we want to have the tightest intervals for all algebraic expressions, i.e. 
;; polynomials in R_i divided by polynomials in R_i.
;; Since not repeating gratuituous R_i's at top that are also in bottom is about finding all the roots,
;; this is obviously a hopeless task. Needless to say, if we split computations into smaller ones, 
;; it becomes even harder to be sure we don't repeat (e.g. if it is a multistep procedure).
