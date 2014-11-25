;; Exercise 1.3

(define (square x) (* x x))

(define (sum-squares x y)
  (+ (square x) (square y)))

(define (sum-largest-two x y z)
  (cond ((and (< x y) (< x z)) (sum-squares y z))
	((and (< y x) (< y z)) (sum-squares x z))
	((and (< z x) (< z y)) (sum-squares x y))))

(sum-largest-two 3 4 5)
(define (abs x)
  (if (> x 0)
    x
    (- x)))

;; Square root from the text with better stopping test.

(define (improve-guess guess x)
  (/ (+ guess (/ x guess)) 2))

(define (good-enough? guess x)
  (< (abs (- (improve-guess guess x) guess)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve-guess guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)
(sqrt 2)

;; Exercise 1.8. all put in one block

(define (cube-root x)
  (define (improve-guess-cube guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough-cube? guess)
    (< (abs (- (improve-guess-cube guess) guess)) 0.001))
  (define (cube-root-iter guess)
    (if (good-enough-cube? guess)
    guess
    (cube-root-iter (improve-guess-cube guess))))
  (cube-root-iter 1.0))
