#lang racket
(require htdp/testing)

;; Ryan Casey
;; csman username: rcasey

;; Assignment 7

;; Part A: Macros

;; Exercise B.1

;; macro for using := to set a variable to a new value
;; (using := instead of set!)
(define-syntax :=
  (syntax-rules () ; no keywords
    ((:= variable value) ; instance
     (set! variable value)))) ;; set value to new value

(define ex1 3)
(:= ex1 10)
(check-expect ex1 10)

;; Exercise B.2

;; macro for using ++ to increment an input variable by 1
(define-syntax ++
  (syntax-rules () ; no keywords
    ((++ variable) ; instance
     (set! variable (+ variable 1))))) ;; set value to new value

;; macro for using *= to set the first variable to the product of the two input variables
(define-syntax *=
  (syntax-rules () ; no keywords
    ((*= x y) ; instance
     (set! x (* x y))))) ;; set value to new value

(define x 3)
(++ x)
(check-expect x 4)
(define y 5)
(*= y x)
(check-expect y 20)

;; Exercise B.3

(define-syntax while
  (syntax-rules ()  ;; no keywords
    ((while test body)
     (do ()  ;; dont care about loop variables
          ((not test) 'done) ;; when test is not true any more, terminate
       body)))) ;; if test still true, do the body of the loop.

 
(define (factorial n)
  (let ((result 1)
        (i 1))
    (while (<= i n)        ; test
           ((*= result i)  ; body
            (++ i)))      ; body
    result))

;; only wanted to do part B


(generate-report)