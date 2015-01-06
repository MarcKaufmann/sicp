;; Exercise 3.39
;; Final outcome depends on whether the top or bottom set! is chosen.
;; If it is top one that comes last, then bottom one must have yielded 11. 
;; So top one can give 121, or 100, the latter if x * x was computed before bottom.
;; If it is the bottom one that gets computed last, that means that x got set to 100
;; So it can yield 101.
;; 11 and 110 are not possible anymore.

;; Exercise 3.40
;; 100: (* x x) gets evaluated, then second gets set, then first gets set.
;; 1,000: Same as above but with p1 and p2 exchanged.
;; 1,000,000: Execute first all of one process, then the other process.
;; (* 1,000 10) -> 10,000
;; (* 10 10 100) -> 10,000
;; (* 10 100 100) -> 100,000
;; In short, all power of 10 between 100 and 1,000,000
;;
;; If it is serialised, then we can only get 1,000,000

;; Exercise 3.43
;; First part seems obvious, since we always have to finish exchanging the amounts in the accounts.
;; The sum across accounts remains the same, since we substract and add respectively a fixed number. That number may be wrong, but adding 20 and substracting 20 keeps sum the same.
;; Changes in total sum can be generated as before, just have some withdraw screw up.

;; Exercise 3.44.
;; Louis is wrong. The reason is that amount never gets reset, so it is the same number no matter what. 
;; As long as the account never gets overdrafted, it is fine.

;; Exercise 3.45
;; The problem is that now withdraw is serialised when exchange is serialised. 
;; Therefore, exchange cannot run withdraw, even though it needs it. It will get blocked.

(define (make-mutex)
  (let ((cell (list false)))
	(define (the-mutex m)
	  (cond ((eq? m 'acquire)
			 (if (test-and-set! cell)
			   (the-mutex 'acquire)))
			((eq? m 'release) (clear! cell))))
	the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
	  true
	  (begin (set-car! cell true)
			 false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
	(lambda (p)
	  (define (serialized-p . args)
		(mutex 'acquire)
		(let ((val (apply p args)))
		  (mutex 'release)
		  val))
	  serialized-p)))
