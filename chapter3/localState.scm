;; Exercise 3.1.

(define (make-accumulator value)
  (define (accumulator addition)
	(if (number? addition)
	    (begin (set! value (+ value addition))
			   value)
	    (error "Can only accumulate numbers -- ACCUMULATOR" addition)))
  accumulator)

;; Exercise 3.2

(define (make-monitored f)
  (define counter 0)
  (define (mf x)
	(cond ((eq? x 'how-many-calls?) 
		   counter)
	      ((eq? x 'reset-count) 
		   (set! counter 0))
		  (else 
			(set! counter (+ counter 1))
			(f x))))
  mf)

;; Exercise 3.3 and 3.7.

(define (make-acc password-on-file balance)
  (define bad-attempts 0)
  (define (dispatch password-entered m)
	(if (correct-password? password-entered)
	    (begin
		  (set! bad-attempts 0)
		  (cond ((eq? m 'withdraw) withdraw)
				((eq? m 'deposit) deposit)
				((eq? m 'check-password) correct-password?)
				(else (error "Can only withdraw from or deposit to account -- MAKE-ACC " (list balance)))))
		;; The following seems hacky. How should one do it? Raising an error is wrong too.
		(begin (set! bad-attempts (+ bad-attempts 1))
			   (if (eq? 3 bad-attempts)
				   (begin 
					 (set! bad-attempts 0)
					 call-the-cops)
				   (lambda (x) "Incorrect password")))))
  (define (withdraw amount)
	(if (< amount balance)
	    (begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))
  ;; Again this stupid hack!!
  (define (call-the-cops x)
	(display "Matafoum, you are so screwed, the cops are on their way.")
	'cops)
  (define (correct-password? password-entered)
	(eq? password-entered password-on-file))
  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)
  dispatch)

;; Woah, this took me 40 minutes... It wasn't even that hard, and I am not sure this is that well done.
;; Quite some duplication of code, too.
;; Action: Try and refactor the whole thing.
(define (make-joint existing-account existing-password new-password)
  ; Action: Go and read how others did it.
  (define (dispatch password message)
	(if (eq? password new-password)
	  (existing-account existing-password message)
	  (lambda (x) "Incorrect password")))
  (if ((existing-account existing-password 'check-password) existing-password)
	dispatch
	"Incorrect password for the existing account."))

(define acc1 (make-acc 'pass 100))
(define acc2 (make-joint acc1 'pass 'secret))


(define (test-make-account)
  (define acc1 (make-acc 'pass 100))
  (define acc2 (make-acc 'pass 100))
  (if (eq? ((acc1 'pass 'withdraw) 10) 90)
	  (display "'withdraw works.")
	  (display "'withdraw does not work."))
  (if (eq? ((acc1 'pass 'deposit) 10) 100)
	  (display "'deposit works.")
	  (display "'deposit does not work."))
  ((acc1 'wrong-password 'deposit) 10)
  ((acc1 'wrong-password 'deposit) 10)
  (if (eq? ((acc2 'wrong-password 'deposit) 10) 'cops)
	  (display "The accounts share a single counter for failed password attempts, which is bad -- TEST-MAKE-ACCOUNT")
	  (display "The accounts do not share a single counter -- TEST-MAKE-ACCOUNT"))
  (list acc1 acc2))

;; Monte-Carlo experiment
;; Idea: Pick two integers at random and check if their gcd is 1. The fraction for which it is, is 6/pi^2.

(define (monte-carlo trials experiment)
  (define (iter-trials trials-remaining trials-passed)
	(cond ((= 0 trials-remaining) 
		   (/ trials-passed trials))
		  ((experiment)
		   (iter-trials (- trials-remaining 1) (+ trials-passed 1)))
		  (else 
		   (iter-trials (- trials-remaining 1) trials-passed))))
  (iter-trials trials 0))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define random-init 104672912)

(define rand
  (let ((x random-init))
	(lambda ()
	  (set! x (rand-update x))
	  x)))

(define (rand-update x) 
  (remainder (* 17 x) (* 23 29)))

;; Exercise 3.5

(define (estimate-integral p x1 y1 x2 y2 trials)
  (define (random-in-range low high)
	(let ((range (- high low)))
	  (+ low (random range))))
  (define (in-rectangle-test)
	(p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials in-rectangle-test)
	 (* (- x2 x1) (- y2 y1))))

(define (in-circle x-center y-center radius)
  (define (p x y)
	(< (+ (square (- x x-center)) (square (- y y-center))) (square radius)))
  p) 

;; Note to self: It is really important to use floats with random, otherwise it only picks integers.
;; If the range is 3 to 8, that means that there will be massive rounding (there can only be 6 values).
;; So the values won't be very good.

(define (test-estimate-integral trials)
  (estimate-integral (in-circle 5. 7. 3.)
					 2. 4.
					 8. 10.
					 trials))

;; Exercise 3.6.
;; let combined with set! totally confuses the hell out of me.
;; OK, so we define a 'constant' rand. What matters is that it points to the same lambda function
;; Thus it always points at the same environment, in which let defines x, then it gets reset.
;; Action: redo this exercise, and something similar, since I screwed it up. 
;; Draw what is going on and being stored?
(define rand 
  (let ((x random-init))
	(define (dispatch message)
	  (cond ((eq? message 'generate)
			 (begin (set! x (random-update x))
					x))
			((eq? message 'reset) 
			 (lambda (new-seed) (set! x new-seed)))
			(else 
			  (error "Unknown message -- RAND" message))))
	dispatch))

;; Exercise 3.8.
;; Not sure the following counts...

(define y 0)

(define (f x)
  (set! y x))

;; Without global variables.
;; Count the number of times f has been called with argument 1 and return that.
;; How to store that in f?
;; Well, I need to create a (non-global) environment and return the equivalen of f from before as new f

(define (g)
  (let ((counter 0))
	(lambda (x)
	  (if (= 1 x)
		  (set! counter (+ counter 1))
		  counter)))
  f)

(define f (g))
