;; Exercise 4.1
;; I don't think that I need nested let's
;; But since I don't know the order of the order of the bindings in let either, play it safe.
;; Actually, maybe I should use define's, since the let's don't necessarily need to be evaluated in that order either?
;; How do I know that the outer let has its argument evaluated first? Doesn't that depend on the implementation?
;; let* would probably be less ambiguous, by necessity.

(define (list-of-values-left exps env)
  (let ((first (eval (first-operand exps) env)))
	(let ((rest (eval (rest-operands exps) env)))
	  (cons first rest))))

;; Exercise 4.2
;; Since application? returns true for all pairs, it will think that 'define' is an application (as that is not checked before-hand any more.
;; The second part with call works by changing application to the following:
(define (application-call? exp)
  (tagged-list? exp 'call))
;; and then having to write tedious code for the rest (and changing the code that gets procedures and so on)

;; TODO: Exercise 4.3
;; Rewrite eval so that the dispatch is done in data-directed style. 
;; The only part that is not clear to me is whether everything should be stored in the same table? 

;; Exercise 4.4.
(define (eval-and list-of-predicates env)
  (define (iter remaining-predicates current-return)
	(if (null? list-of-predicates) 
		current-return
		(let ((first-predicate (eval (car remaining-predicates) env)))
		  (if (true? first-predicate)
			  (iter (cdr remaining-predicates) first-predicate)
			  false))))
  (iter list-of-predicates true))
;; And similar for or.
;; TODO: Implement this as a derived expression, using if's.

;; Exercise 4.5.

;; Exercise 4.14
;; OK, I did not figure this one out by myself, rather than write down the answer, let's see why I did not.
;; I only thought in my head, without writing down. More importantly, I did not bother to check the obvious:
;; What does (map proc bla) do? If I did that and wrote it out, no way of not getting it.
;; In short, do empirics.


