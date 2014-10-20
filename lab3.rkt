#lang racket
(require htdp/testing)

;; Ryan Casey
;; csman username: rcasey

;; Assignment 3

;; Part A: Abstraction and cons

;; Exercise A.1
;; SICP 2.2
;; make-point: int int -> point
(define (make-point x y)
  (cons x y))
;; x-point: point -> int
(define (x-point p1)
  (car p1))
;; y-point: point -> int
(define (y-point p1)
  (cdr p1))
;; make-segment: point point -> segment
(define (make-segment p1 p2)
  (cons p1 p2))
;; start-segment: segment -> point
(define (start-segment s1)
  (car s1))
;; end-segment: segment -> point
(define (end-segment s1)
  (cdr s1))
;; midpoint-segment: segment -> point
(define (midpoint-segment s1)
  (make-point (/ (+ (x-point (start-segment s1)) (x-point (end-segment s1))) 2)
      (/ (+ (y-point (start-segment s1)) (y-point (end-segment s1))) 2)))
;; square: int -> int
(define (square x)
  (* x x))
;;segment-length: segment -> int
(define (segment-length s1)
  (let ((d1 (- (x-point (end-segment s1)) (x-point (start-segment s1))))
        (d2 (- (y-point (end-segment s1)) (y-point (start-segment s1)))))
    (sqrt (+ (square d1) (square d2)))))
                       
  (define p1 (make-point 0.0 0.0))
  (define p2 (make-point 10.0 0.0))
  (define p3 (make-point 10.0 10.0))
  (define s1 (make-segment p1 p2))
  (define s2 (make-segment p2 p3))
  (define s3 (make-segment p3 p1))
  (check-within (segment-length s1) 10.0 0.00001)
  (check-within (segment-length s2) 10.0 0.00001)
  (check-within (segment-length s3) 14.14213 0.00001)
  (check-within (x-point (midpoint-segment s3)) 5.0 0.00001)
  (check-within (y-point (midpoint-segment s3)) 5.0 0.00001)
  
;; Exercise A.2
;; SICP 2.3
  
;; first construction, using Lower Left and Upper Right points to define rect.
;; make-rectangle: point point -> rectangle
(define (make-rectangle ll ur) 
  (cons ll ur))
;; rectangle-lower-segment: rectangle -> segment
(define (rectangle-lower-segment r)
  (make-segment (car r) (make-point (car (cdr r)) (cdr (car r)))))
;; rectangle-upper-segment: rectangle -> segment
(define (rectangle-upper-segment r)
  (make-segment (make-point (car (car r)) (cdr (cdr r))) (cdr r)))
;; rectangle-left-segment: rectangle -> segment
(define (rectangle-left-segment r)
  (make-segment (car r) (make-point (car (car r)) (cdr (cdr r)))))
;; rectangle-right-segment: rectangle -> segment
(define (rectangle-right-segment r)
  (make-segment (make-point (car (cdr r)) (cdr (car r))) (cdr r)))

;; second construction, using lower x, lower y, upper x, upper y
;; make-rectangle2: integer integer integer integer -> rectangle
(define (make-rectangle2 lx ly ux uy)
  (cons (cons lx ly) (cons ux uy)))
;; rectangle-lower-segment2: rectangle -> segment  
(define (rectangle-lower-segment2 r)
  (make-segment (make-point (car (car r)) (cdr (car r))) 
                (make-point (car (cdr r)) (cdr (car r)))))
;; rectangle-upper-segment2: rectangle -> segment  
(define (rectangle-upper-segment2 r)
  (make-segment (make-point (car (car r)) (cdr (cdr r))) 
                (make-point (car (cdr r)) (cdr (cdr r)))))
;; rectangle-left-segment2: rectangle -> segment  
(define (rectangle-left-segment2 r)
  (make-segment (make-point (car (car r)) (cdr (car r))) 
                (make-point (car (car r)) (cdr (cdr r)))))
;; rectangle-right-segment2: rectangle -> segment  
(define (rectangle-right-segment2 r)
  (make-segment (make-point (car (cdr r)) (cdr (car r))) 
                (make-point (car (cdr r)) (cdr (cdr r)))))

;; for first construction of rectangle (2 corner points)
;; rectangle-area: rectangle -> integer
(define (rectangle-area r)
  (* (segment-length (rectangle-lower-segment r))
     (segment-length (rectangle-left-segment r))))
;; rectangle-perimeter: rectangle -> integer
(define (rectangle-perimeter r)
  (+ (* 2 (segment-length (rectangle-lower-segment r)))
     (* 2 (segment-length (rectangle-left-segment r)))))

;; for second construction of rectangle (using lower x, lower y, upper x, 
;; upper y)
;; rectangle-area: rectangle -> integer
(define (rectangle-area2 r)
  (* (segment-length (rectangle-lower-segment2 r))
     (segment-length (rectangle-left-segment2 r))))
;; rectangle-perimeter: rectangle -> integer
(define (rectangle-perimeter2 r)
  (+ (* 2 (segment-length (rectangle-lower-segment2 r)))
     (* 2 (segment-length (rectangle-left-segment2 r)))))

(define p4 (make-point 0.0  2.0))
(define p5 (make-point 5.0 10.0))
(define r (make-rectangle p4 p5))             ;; first representation
;; second representation: lx, ly, ux, uy
(define r2 (make-rectangle2 0.0 2.0 5.0 10.0)) 
(check-within (rectangle-area r)      40.0 0.0001)
(check-within (rectangle-perimeter r) 26.0 0.0001)
;; checks for 2nd representation
(check-within (rectangle-area2 r2)      40.0 0.0001)
(check-within (rectangle-perimeter2 r2) 26.0 0.0001)

;; Exercise A.3
;; SICP 2.4
(define (new_cons x y)
  (lambda (m) (m x y)))

(define (new_car z)
  (z (lambda (p q) p)))
;; Evaluate (new_car (new_cons x y)):
;; evaluate (new_cons x y)
;;   evaluate new_cons -> (lambda (m) (m x y))
;;   apply x, y -> (lambda (m) (m x y))
;;    so, (new_cons x y) -> (lambda (m) (m x y))
;;     evaluate (new_car (lambda (m) (m x y)):
;;      evaluate new_car -> (z (lambda (p q) p)):
;;       -> ((z (lambda (p q) p)) (lambda (m) (m x y))
;;          substitute (lambda (m) (m x y)) for z:
;;           -> ((lambda (m) (m x y)) (lambda (p q) p))
;;             substitute (lambda (p q) p) for m:
;;               (Note that m evaluates to p in the second lambda expression)
;;               -> ((lambda (p q) p) x y)
;;                  substitute x, y for p, q:
;;                    -> (lambda (x y) x)
;;                      -> evaluate lambda (x y) x) -> x.
;;                          Therefore, (new_car (new_cons x y)) -> x, as desired

(define (new_cdr z)
  (z (lambda (p q) q)))
;; Evaluate (new-cdr (new-cons 1 2)):
;; evaluate 1 -> 1
;; evaluate 2 -> 2
;; evaluate new_cons -> (lambda (x y) 
;;                        (lambda (m) (m x y)))
;; apply (lambda (x y) 
;;         (lambda (m) (m x y))) to 1, 2
;;   substitute 1 for x, 2 for y into (lambda (m)...
;;     -> (lambda (m) (m 1 2))
;; evaluate new-cdr -> (lambda (z) 
;;                        (z (lambda (p q) q)))
;; apply (lambda (z) 
;;         (z (lambda (p q) q))) to (lambda (m) (m 1 2))
;; (Note: here we are substituting a function in for z)
;;   substitute (lambda (m) (m 1 2)) for z into (z (lambda (p...
;;     evaluate (lambda (m) (m 1 2) (lambda (p q) q):
;;        apply (lambda (m)
;;                 (m 1 2) to (lambda (p q) q)
;;           substitute (lambda (p q) q) in for m in (m 1 2):
;;             (Note: here we are substituing a function in for m)
;;             -> ((lambda (p q) q) 1 2)
;; evaluate ((lambda (p q) q) 1 2):
;;    apply (lambda (p q) q) to 1, 2:
;;     substitute 1 for p, 2 for q in q:
;;      -> 2
;;  Result: 2


;; Exercise 2.4
;; SICP 2.5
;; cons: int int -> int
(define (cons-new a b)
  (* (expt 2 a)
     (expt 3 b)))

;; Note that 2^n is always an even number, and 3^n is always odd.
;; Therefore, we can decompose (2^a)*(3^b) = n by finding the largest a possible
;; by dividing out the maximum number of 2's from n.
  
;; times-divisble: int int -> int
(define (times-divisible n d)
  (iter-div n d 0))

;; iter-div: int int int -> int
;; helper function for times-divisble
(define (iter-div x d n)
  (if (= 0 (remainder x d))
     (iter-div (/ x d) d (+ 1 n))
        n))
;; car-new: int -> int
(define (car-new x)
  (times-divisible x 2))
;; cdr-new: int -> int
(define (cdr-new x)
  (times-divisible x 3))

(check-expect (car-new (cons-new 5 7)) 5)
(check-expect (cdr-new (cons-new 5 7)) 7)

;; Exercise A.5
;; SICP 2.6

(define zero (lambda (s) (lambda (z) z)))
(define one (lambda (s) (lambda (z) (s z))))
(define two (lambda (s) (lambda (z) (s (s z)))))
(define three (lambda (s) (lambda (z) (s (s (s z))))))
(define four (lambda (s) (lambda (z) (s (s (s (s z)))))))
(define five (lambda (s) (lambda (z) (s (s (s (s (s z))))))))
(define six (lambda (s) (lambda (z) (s (s (s (s (s (s z)))))))))
(define seven (lambda (s) (lambda (z) (s (s (s (s (s (s (s z))))))))))
(define eight (lambda (s) (lambda (z) (s (s (s (s (s (s (s (s z)))))))))))
(define nine (lambda (s) (lambda (z) (s (s (s (s (s (s (s (s (s z))))))))))))
(define ten (lambda (s) (lambda (z) (s (s (s (s (s (s (s (s (s (s z)))))))))))))

;; Church-to-integer: ChurchNumeral -> int
(define (Church-to-integer f)
  ((f (lambda (x)
        (+ x 1))) 0))

;; add: ChurchNumeral ChurchNumeral -> ChurchNumeral
(define (add m n)
   (lambda (s) (lambda (x) ((m s) ((n s) x)))))

;; add=1: ChurchNumeral -> ChurchNumeral
(define (add-1 n)
  (lambda (s) (lambda (z) (s ((n s) z)))))

(check-expect (Church-to-integer zero)   0)
(check-expect (Church-to-integer one)    1)
(check-expect (Church-to-integer two)    2)
(check-expect (Church-to-integer three)  3)
(check-expect (Church-to-integer four)   4)
(check-expect (Church-to-integer five)   5)
(check-expect (Church-to-integer six)    6)
(check-expect (Church-to-integer seven)  7)
(check-expect (Church-to-integer eight)  8)
(check-expect (Church-to-integer nine)   9)
(check-expect (Church-to-integer ten)   10)

(check-expect (Church-to-integer (add zero three)) 3)
(check-expect (Church-to-integer (add one  three)) 4)
(check-expect (Church-to-integer (add two  three)) 5)
(check-expect (Church-to-integer (add three zero)) 3)
(check-expect (Church-to-integer (add three one))  4)
(check-expect (Church-to-integer (add three two))  5)


;; Part B: Working with Lists

;; Exercise B.1
;; SICP 2.17
;; last-pair: list -> last-element-of-list
(define (last-pair lst)
  (cond ((null? lst) (error "last-pair -- invalid list (contains no elements)"))
        ((null? (cdr lst)) (list (car lst)))
        (else (last-pair (cdr lst)))))

(check-expect (last-pair (list 23 72 149 34)) (list 34))
(check-expect (last-pair (list 1 1 4 1)) (list 1))
(check-error (last-pair (list)) "last-pair -- invalid list (contains no elements)")

;; Exercise B.2
;; SICP 2.18
;; reverse: list -> list
(define (reverse lst)
  (iter-reverse lst (list)))

;; iter-reverse: list start-list -> reversed-list
(define (iter-reverse lst current)
  (if (null? lst)
      current
      (iter-reverse (cdr lst)(cons (car lst) current))))
            ;; if a is nonempty, add the first element of a to the current list,
              ;; and run the iteration again on the rest of the list (after 1st)
;; Note that the iterative definition makes this process linear in time O(n), 
;; where n is the size of the input list.

(check-expect (reverse (list 23 72 149 34)) (list 34 149 72 23))
(check-expect (reverse (list)) (list))

;; Exercise B.3
;; SICP 2.21
;; square-list: list -> list-with-items-squared
(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))

(check-expect (square-list (list 2 1 5 9)) (list 4 1 25 81))

;; square-list2: list -> list-with-items-squared
(define (square-list2 items)
  (map square items))
(check-expect (square-list2 (list 2 1 5 9)) (list 4 1 25 81))

;; Exercise B.4
;; SICP 2.22

;;(define (square-list items)
;;  (define (iter things answer)
;;    (if (null? things)
;;        answer
;;        (iter (cdr things) 
;;              (cons (square (car things))
;;                    answer))))
;;(iter items nil))
;; This doesn't work because (cdr things) gives the rest of the list (after the
;; first element, while (cons (square (car things)) answer) adds (square (car things))
;; to the beginning of answer.  So we go through the list, first element ->
;; last element, and each time add the new squared number to the front of the 
;; result list called "answer".  Therefore, the last element we read from the 
;; input list will be at the front of the resulting squared list, and thus the 
;; list will be reversed.

;;(define (square-list items)
;;  (define (iter things answer)
;;    (if (null? things)
;;        answer
;;        (iter (cdr things)
;;              (cons answer
;;                    (square (car things))))))
;; (iter items nil))
;; Note that here we flipped the arguments to cons.  Now, we are trying to 
;; add the current list called "answer" to the single element 
;; (square (car things)).  Since (square (car things) isn't a list, we try to 
;; make a new list every time. The result is a bunch of lists inside lists, 
;; which we don't want.  The first argument of the cons should be the element 
;; that we are trying to add the the list, and the second argument should be the
;; list.
;; Ex. (square-list (list 1 2 3 4)) -> '((((() . 1) . 4) . 9) . 16)

;; Exercise B.5
;; SICP 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
;; (append x y)
;;   -> '(1 2 3 4 5 6)
;; (cons x y)
;;   -> '((1 2 3) 4 5 6)
;; (list x y)
;;   -> '((1 2 3) (4 5 6))


;; Exercise B.6

;; neg-elements: list -> int
;; returns the number of negative elements of a list
(define (neg-elements items)
  (neg-elements-iter items 0))
  
;; neg-elements-iter: list int -> int
;; helper function for neg-elements
(define (neg-elements-iter items current)
  (cond ((null? items) 0)
        ((null? (cdr items)) current)
        ((< (car items) 0) (neg-elements-iter (cdr items) (+ 1 current)))
        (else (neg-elements-iter (cdr items) current))))

(check-expect (neg-elements (list 23 72 149 34)) 0)
(check-expect (neg-elements (list -23 72 0 34)) 1)
(check-expect (neg-elements (list -23 72 -0.89 0)) 2)
(check-expect (neg-elements (list)) 0)

;; powers: int -> list
;; returns a list of all powers of 2 up to n (where n is a positive integer)
(define (powers n)
  (if (< n 0) 
      (error "powers -- invalid integer (need a positive integer)")
      (powers-iter (- n 1) (list))))
;; powers-iter: int list -> list
;; helper function for powers
(define (powers-iter n current)
  (cond ((< n 0) current)
        (else (powers-iter (- n 1) (cons (expt 2 n) current)))))

(check-expect (powers 4) (list 1 2 4 8))
(check-expect (powers 0) (list))
(check-error (powers -1) "powers -- invalid integer (need a positive integer)")

;; prefix-sum: list -> list
;; returns a list that is the sum of all elements up to each place in the input list
(define (prefix-sum items)
  (if (null? items)
      (list)
      (prefix-sum-iter items (list) 0)))

;; prefix-sum-iter: list list int -> list
;; helper function for prefix-sum
(define (prefix-sum-iter items current running-total)
    (cond ((null? items) (reverse current))
        (else (prefix-sum-iter (cdr items) (cons (+ running-total (car items)) 
                                      current) (+ running-total (car items))))))
  
(check-expect (prefix-sum (list 1 3 5 2)) (list 1 4 9 11))
(check-expect (prefix-sum (list 0 3 -1 7)) (list 0 3 2 9))
(check-expect (prefix-sum (list)) (list))

;; Exercise B.7
;; part 1
(define (make-result x y) (cons x y))
(define (get-x result) (car result))
(define (get-y result) (cdr result))
(define (find-max-y a-list)
  (define (find-max-y-helper current-list current-max-y)
    (let ((new-max-y (if (null? current-list) 
                         0.0 (car current-list))))    ;; use a let statement
      (cond ((null? current-list) current-max-y)
        ((< (get-y new-max-y) current-max-y) ;; use defined new-max-y
         (find-max-y-helper (cdr current-list)
                            current-max-y))
        (else (find-max-y-helper (cdr current-list)
                          (get-y new-max-y))) ))) ;; use defined new-max-y
  (find-max-y-helper a-list  0.0)) ;; assume max > 0.0

(check-expect (find-max-y (list (cons 1 2) (cons 2 6) (cons 7 -1))) 6)

;; Note that in the original code for find-max-y, (cdr current-list) was written
;; out twice.  However, the two cases in which they were written are mutually 
;; exclusive, so we can only go into one of them each time through the recursive
;; call.  Thus, repeating the code twice doesn't actually slow down the code at
;; all, since one occurs in an else case while one occurs inside one of the
;; conditional expressions.

;; part 2
(define (find-x-for-max-y a-list)
    (find-x-max-y a-list))

(define (find-x-max-y a-list)
  (define (find-x-max-y-helper current-list current-max-y current-max-x)
    (let ((new-max-y (if (null? current-list) 
                         0.0 (car current-list))))    ;; use a let statement
      (cond ((null? current-list) current-max-x)
        ((< (get-y new-max-y) current-max-y) ;; use defined new-max-y
         (find-x-max-y-helper (cdr current-list)
                            current-max-y current-max-x))
        (else (find-x-max-y-helper (cdr current-list)
                          (get-y new-max-y) (get-x new-max-y))) ))) ;; use defined new-max-y
  (find-x-max-y-helper a-list  0.0 0.0)) ;; assume max > 0.0

(define (find-x-with-y  a-list  y)
  (cond ((null? a-list)
         (error "No y found to match target y =" y))
        ((= (get-y (car a-list))  y)
            (get-x (car a-list)))
        (else
          (find-x-with-y  (cdr a-list)  y))))

(check-expect (find-x-for-max-y (list (cons 1 2) (cons 2 6) (cons 7 -1))) 2)
(check-expect (find-x-for-max-y (list (cons 1 2) (cons 2 6) (cons 5 6))) 5)
(check-expect (find-x-for-max-y (list (cons 1 2) (cons 2 6) (cons 100 99))) 100)
(check-within (find-x-for-max-y (list)) 0.0 0.00001)



;; Exercise B.8
;; SICP 2.27
 
;; deep-reverse: list -> list-deep-reversed
(define (deep-reverse items)
  (if (null? items)
      items
    (iter-deep-reverse items (list))))

;; iter-deep-reverse: list list-start -> list-deep-reversed
;; helper function for deep-reverse.
(define (iter-deep-reverse lst current)
  (if (null? lst)
      current
      (if (list? (car lst))
          ;; if we find a sublist, run deep-reverse on that sublist before
          ;; adding it to the deep-reversed list.
          (iter-deep-reverse (cdr lst)(cons (deep-reverse (car lst)) current))
      (iter-deep-reverse (cdr lst)(cons (car lst) current)))))


(check-expect (deep-reverse (list)) (list))
(check-expect (deep-reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-expect (deep-reverse (list (list 1 2) (list 3 4) 5)) (list 5 (list 4 3) (list 2 1)))

(generate-report)