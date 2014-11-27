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

;; Exercise 1.15
;; a) It is applied as many times as we have to divide 12.15 by 3 to go below 0.1, i.e. five times.
;; b) log(a)

;; Exercise 1.16
;; Write an iterative procedure for fast exponentiation.

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-exp b n)
  (define (iter b n acc)
    (cond ((= n 0) acc)
          ((even? n) (iter (* b b) (/ n 2) acc))
          (else (iter b (- n 1) (* acc b)))))
  (iter b n 1))

;; Exercise 1.17 and 1.18 (I don't bother with 1.17 actually, don't do the recursive.)
;; Write an iterative procedure for fast multiplication, based on addition, double and halve only.
;; Obviously this requires those to exist, so I use multiplication for those.
;; In binary, this would simply be a bitshift.

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (mult a b)
  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter (double a) (halve b) acc))
          (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ test-divisor 2)))))
  (if (divides? 2 n)
      2
      (find-divisor 3)))

;; Exercise 1.22

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n number-found)
  (newline)
  (display n)
  (start-prime-test n (runtime) number-found))

(define (start-prime-test n start-time number-found)
  (if (prime? n)
      (report-prime (- (runtime) start-time) (+ n 2) (+ number-found 1))
      (timed-prime-test (+ n 2) number-found)))

(define (report-prime elapsed-time n number-found)
  (display " *** ")
  (display elapsed-time)
  (if (< number-found 3)
      (timed-prime-test n number-found)
      (display " Done ")))

(define (find-three-primes start)
  (if (even? start)
      (timed-prime-test (+ start 1) 0)
      (timed-prime-test start 0)))

;; Exercise 1.25.
;; Answer: No, that would almost surely be bad. a^p can get hellishly large if we don't take remainder wrt p and most numbers will not fit into memory any more. E.g. p roughly 10^10 gives us that a random number a will lead to (10^10)^(10^10), which is ridiculous! This number has 10^11 zeroes, ie is bigger than 2^32, and that's for primes of the order of billions. Go into billions of billions and you fill all the universe with zeroes rather quickly.


