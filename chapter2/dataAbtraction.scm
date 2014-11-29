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
