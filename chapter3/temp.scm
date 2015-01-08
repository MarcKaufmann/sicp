;; Exercise 3.52
;; (define sum 0) =>   sum -> 0
;; (define seq (stream-map accum (stream-enumerate-interval 1 20))) => sum -> 1
;; (define y (stream-filter even? seq)) => sum -> 1 + 2, since we are running the stream to the first even number
;; (define z ...) => sum -> 1 + 2 + 3 + 4 + 5, since we go up to the first multiple of 5. 
;; (stream-ref y 7) => sum -> 1 + ... + 8 (give or take 1, if I got the offset wrong)
;; (display-stream z) => sum -> 1 + ... + 8, since it should not be changed by displaying.
;; The last line is true only under memoization, I believe. 
;; If we did not memoize, then the stream would change on each evalutaion? Check this.

;; Exercise 3.53
;; It is the same as double, i.e. it is the sequence of powers of 2.

;; Exercise 3.54
(define factorials (cons-stream 1 (mul-streams integers factorials)))

;; Exercise 3.55
(define (partial-sums s) 
  ;; Should I use let here? What is more lispy/schematic?
  ;; ps(n + 1) = ps(n) + s(n + 1)
  ;; ps(0) = s(0)
  ;; ps = (cons-stream s(0) (add-streams ps (stream-cdr s)))
  (define ps (cons-stream (car s) (add-streams ps (stream-cdr s))))
  ps)

;; Exercise 3.56
(define S (cons-stream 1 (merge (scale-stream S 2)
								(merge (scale-stream S 3)
									   (scale-stream S 5)))))
