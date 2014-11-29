;;; Section 1.3.
;;; Formulating Abstractions with Higher-Order Procedures

;; An iterative version of sum

(define (sum f from next to)
  (define (iter current acc)
    (if (> current to)
        acc
        (iter (next current) (+ acc (f current)))))
  (iter from 0))

(define (square x) (* x x))

(define (^ x n) 
  (define (iter x n acc)
    (cond ((= n 0) acc)
          ((even? n) (iter (square x) (/ n 2) acc))
          (else (iter x (- n 1) (* acc x)))))
  (iter x n 1))

(define (identity x) x)

(define (inc x) (+ x 1))

;; TODO: Not sure this is really a gain in abstraction, feels wrong.
;; I still have to pass from and to in the definition of sum-cubes, which seems a waste of time.

(define (sum-nth-power n from to )
  (define (nth-power x)
    (^ x n))
  (sum nth-power from inc to))

(define (sum-cubes from to)
  (sum-nth-power 3 from to))

(define (integral f a b dx)
  (define (add-dx a) (+ a dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (f-simpson k)
    (cond ((= k 0) (f a))
          ((= k n) (f b))
          ((even? k) (* 2 (f (+ a (* k h)))))
          (else (* 4 (f (+ a (* k h)))))))
  (* (/ h 3)
     (sum f-simpson 0 inc n)))

;; Exercise 1.30
;; Done, since I wrote the functions interatively from the start.

;; Exercise 1.31

(define (product f from next to)
  (define (iter current acc)
    (if (> current to)
        acc
        (iter (next current) (* acc (f current)))))
  (iter from 1))

(define (product-rec f from next to)
  (if (> from to)
      1
      (* (f from) 
         (product-rec f (next from) next to))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (f k)
    (/ (* (- k 1) (+ k 1))
       (square k)))
  (define (add-two x) (+ x 2))
  (* 4 (product f 3.0 add-two (inc (* 2 n)))))

;; Exercise 1.32

(define (accumulate-rec combiner null-value f from next to)
  (if (> from to)
      null-value
      (combiner (f from) (accumulate-rec combiner null-value f (next from) next to))))

(define (accumulate combiner null-value f from next to)
  (define (iter current acc)
    (if (> current to)
        acc
        (iter (next current) (combiner acc (f current)))))
  (iter from null-value))

(define (sum-acc f from next to)
  (accumulate + 0 f from next to))

(define (product-acc f from next to)
  (accumulate * 1 f from next to))

;; Exercise 1.33

(define (filtered-accumulate p combiner null-value f from next to)
  (define (iter current acc)
    (cond ((> current to) acc)
          ((p current) (iter (next current) (combiner acc (f current))))
          (else (iter (next current) acc)))) 
  (iter from null-value))

(define (smallest-divisor n)
  (define (divides? a b)
    (= 0 (remainder b a)))
  (define (find-divisor a test-divisor)
    (cond ((> (square test-divisor) a) a)
          ((divides? test-divisor a) test-divisor)
          (else (find-divisor a (+ test-divisor 2)))))
  (if (divides? 2 n)
      2 
      (find-divisor n 3)))

(define (prime? n)
  (= (smallest-divisor n) n))

; Now I can define the sum of squares of primes in an interval a to b
; Holy moly, that's short. And it seems to work.

(define (sum-squares-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; This use of lambda is cool!

((lambda (x y z) (+ x y (square z))) 1 2 3)

;; Exercise 1.34

; Define f as follows:

(define (f g)
  (g 2))

; What is (f f)? It is (f 2), which is (2 2) which is garbage? In the repl this is also garbage.

(define (average a b) (\ (+ a b) 2))

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))
              
;; Fixed point

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (let ((next (f first-guess)))
    (if (close-enough? first-guess next)
        first-guess
        (fixed-point f next))))

(define (fixed-point-damp- f first-guess )
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (let ((next (average (f first-guess) first-guess)))
    (if (close-enough? first-guess next)
        first-guess
        (fixed-point f next))))

(define (fixed-point-counter f first-guess counter)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (let ((next (f first-guess)))
    (display counter)
    (newline)
    (display first-guess)
    (newline)
    (if (close-enough? first-guess next)
        first-guess
        (fixed-point f next (inc counter)))))

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point-damp-counter f first-guess counter)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (let ((next (average (f first-guess) first-guess)))
    (display first-guess)
    (newline)
    (if (close-enough? first-guess next)
        first-guess
        (fixed-point f next (inc counter)))))

;; Exercise 1.35
;; Let g be the golden ratio. The geometric definition of g is that if we have a rectangle
;; of sides g and 1, if we take out a square of side 1, the rectangle that remains has also
;; sides with ratio g. Thus g is given by 
;; 1/(g - 1) = g/1 <=> 1 = g^2 - g <=> 1/g = g - 1 <=> 1/g + 1 = g
;; Just run (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0) to get the value.

;; Exercise 1.36.
;; The non-damped version takes 34 steps, the other 32. It's only in the beginning that it seems to matter,
;; when there are large swings.

(define (ex-1-36)
  (fixed-point (lambda (x) (/ (log 1000.0) (log x))) 2.0 1))

(define (ex-1-36-damp)
  (fixed-point-damp (lambda (x) (/ (log 1000.0) (log x))) 2.0 1))

;; Exercise 1.37

(define (cont-frac-rec n d k)
  (let (( D (cont-frac-rec (lambda (i) (n (inc i))) 
                       (lambda (i) (d (inc i)))
                       (- k 1))))
    ((/ (n 1) (+ (d 1) D)))))

(define (cont-frac n d k)
  (define (iter k acc)
    (if (= k 0)
        acc
        (let ((new-acc (/ (n k) (+ (d k) acc))))
	  (iter (- k 1) new-acc))))
  (iter k 0))

;; This gives an accurate value for phi for 4 decimals for k=11.

(define (golden k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;; Exercise 1.38.
;; (d i) is 1 if i is 1 or 0 modulo three. It is equal to 2*k when i is 3*k - 1.

(define (d-euler i)
  (if (= 2 (remainder i 3)) 
      (* 2.0 (/ (inc i) 3))
      1.0))

(define (e k)
  (+ (cont-frac (lambda (i) 1.0) 
                d-euler
                k) 2.0))

;; Newton's method

(define dx 0.00001)

(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x 
       (/ (g x) ((deriv g) x)))))

(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (cube-root x)
  (fixed-point-of-transform (lambda(y) (/ x (square y))) average-damp 1.0))

;; Exercise 1.40

(define (cube x) (* x x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (find-zero-cubic a b c)
  (fixed-point-of-transform (cubic a b c) newton-transform 1.0))

;; Exercise 1.41
;; Note that (double (double double)) = (double double) (double double) = (double (double (double (double))))
;; That totally confused me.

(define (double g)
  (lambda (x) (g (g x))))

;; Exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43

;; Not the fastest implementation, since it is O(n), and I could do it in O(log(n))

(define (repeated-rec f n)
  (lambda (x) (compose f (repeated f (- n 1)))))

;; Faster and iterative.

(define (repeated f n)
  (define (iter f f-acc k)
    (cond ((= k 0) f-acc)
          ((even? k) (iter (compose f f) f-acc (/ k 2)))
          (else (iter f (compose f f-acc) (- k 1)))))
  (define (identity x) x)
  (iter f identity n))

;; Exercise 1.44: Not tested.

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (repeated-smooth f n)
  ((repeated smooth n) f))

;; Exercise 1.45

(define (^ n x)
  (if (= n 0)
      1
      (* x (^ (- n 1) x))))

(define (nth-root-repeat x n k)
  (fixed-point-of-transform (lambda (y) (/ x (^ (- n 1) y))) (repeated average-damp k) 1.0))

(define (nth-root x n)
  (nth-root-repeat x n (floor (/ (log n) (log 2)))))

;; Exercise 1.46.

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess) 
    (if (good-enough? guess)
        guess
        (iter (improve-guess guess))))
  iter)

(define tolerance 0.00001)

(define (good-enough? x)
  (lambda (guess) 
    (< (abs (- (square guess) x)) tolerance)))

(define (improve-sqrt x)
  (lambda (guess)
    (average guess (/ x guess))))

(define (sqrt-improve x)
  ((iterative-improve (good-enough? x) (improve-sqrt x)) 1.0))

; This is way hard getting right. 

(define (fixed-point-improve f)
  (let ((good-enough? (lambda (guess next-guess)
                        (< (abs (- guess  next-guess)) tolerance)))
        (improve-guess (lambda (guess) (f guess))))
       ((iterative-improve (lambda (guess) (good-enough? guess (improve-guess guess))) improve-guess) 1.0)))
