#lang r5rs
(define (copy-list x)
  (if (null? x) 
      (list)
      (cons (car x) (copy-list (cdr x)))))

(define (bash-cdr! x)
  (if (null? x) 
      x
      (begin
        (set-cdr! (car x) (caar x))
        (bash-cdr! (cdr x)))))

(define a (list (cons 1 2) (cons 3 4) (cons 5 6)))
(define b (copy-list a))
(bash-cdr! b)
a ;; this is w