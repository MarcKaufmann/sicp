;;; Hierarchical Data and Closure Property

;; Exercise 2.17.

(define (last-pair l)
  ;; No idea what I should output if l is empty. Empty list or error?
  (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))

;; Exercise 2.18.
;; I couldn't figure this one out without using append, or rewriting it.
;; Why is that? Is there a way to do this smarter?

(define (reverse l)
  ;; Again, not dealing with the empty list
  (if (null? (cdr l))
      (list (car l))
      (append (reverse (cdr l)) 
              (list (car l)))))
;; Yes, I can do better, by checking whether cdr l is empty or null, or a singleton or something like that.

;; Exercise 2.19.

;; Exercise 2.20.

;; Not the prettiest way of doing it, I'm sure.
(define (same-parity . l)
  (define p 
    (if (even? (car l))
        even?
        (lambda (x) (not (even? x)))))
  (define (get-same-parity l)
    (if (null? l)
        (list)
        (if (p (car l))
            (cons (car l) (get-same-parity (cdr l)))
            (get-same-parity (cdr l)))))
  (get-same-parity l))
