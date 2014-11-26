;;; Section 1.2: Procedures and Processes They Generate

;; Recursive Factorial

(define (fac n)
  (if (= n 1)
      1
      (* n (fac (- n 1)))))

;; Iterative Factorial

(define (fact-iter product counter max-counter)
  (if (= counter max-counter)
      (* counter product)
      (fact-iter (* counter product) (+ counter 1) max-counter)))

(define (factorial n)
  (fact-iter 1 1 n))

;; Exercise 1.9
;; The first is linear recursive, since after k iterations it is (inc^k (dec^k(a),b)). 
;; The second is iterative, since after k iterations, it's (+ (dec^k(a) inc^k(b)))

;; Fibonacci. I assume that the series starts with n=1, not n=0.

;; Recursive Process.

(define (fib n)
  (if (< n 3)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

;; Iterative version.

(define (fibonacci n)
  ;; fib1 is the (n-1)th and fib2 the (n-2)th fibonacci number
  (define (iter fib1 fib2 counter)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          ((= n counter) (+ fib1 fib2))
          (else (iter (+ fib1 fib2) fib1 (+ counter 1)))))
  (iter 1 0 2))

;; TODO: Count change example. Do it without looking at the text.

;; TODO: Write an iterative example of the count change.

;; Exercise 1.11: f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3), and f(n) = n when n < 3. 
;; Write iterative and recursive solutions for f. (I only bothered with the iterative.)

(define (f n)
  (define (next-f f1 f2 f3)
    (+ f1 (* 2 f2) (* 3 f3)))
  (define (iter f1 f2 f3 counter)
    (cond ((< n 3) n)
          ((= n counter) f1)
          (else (iter (next-f f1 f2 f3) f1 f2 (+ counter 1)))))
  (iter 0 1 2 2))

;; Exercise 1.12: Write a procedure that computes the elements of Pascal's triangle
;; The relationship for p(n,r), where n is the 'row' and r is the 'column' is
;; p(n,r) = p(n-1, r-1) + p(n-1,r)

(define (p n r)
  (if (or (= n 0) (= n r) (= r 0))
      1
      (+ (p (- n 1) r) (p (- n 1) (- r 1)))))

;; TODO: Exercise 1.13: Prove that the nth Fibonacci number is the closest integer to phi^n/sqrt(5), phi the golden ratio.

;; TODO: Write tests for all the exercises. I have not done so yet.
