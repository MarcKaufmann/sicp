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
