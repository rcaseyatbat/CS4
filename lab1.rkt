#lang racket

;;
;; Part A:Basic Exercises

;; Exercise A.1 
;; 10
;; -> 10

;; (+ 5 3 4)
;; -> 12

;; (- 9 1)
;; -> 8

;; (/ 6 2)
;; -> 3

;; (+ (* 2 4) (- 4 6)) 
;; -> 6

;; (define a 3)
;; (define b (+ a 1))
;; (+ a b (* a b))
;; -> 19

;; (= a b) 
;; -> #f

;; (if (and (> b a) (< b (* a b)))
;;  b
;;  a)
;; -> 4

;; (cond ((= a 4) 6)
;;    ((= b 4) (+ 6 7 a))
;;    (else 25))
;; -> 16

;; (+ 2 (if (> b a) b a))
;; -> 6

;; (* (cond ((> a b) a)
;;       ((< a b) b)
;;       (else -1))
;; (+ a 1))
;; -> 16


;; Exercise A.2
;; the Scheme expression in prefix notation:
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) (* 3 (- 6 2) (- 2 7)))
;; -> -37/150

;; Exercise A.3
(define (sum-of-squares-of-two-largest x y z)
  (define a (if (> x y) x y))
  (define b (if (= a x) (if (> y z) y z) (if (> x z) x z)))
  ( + (* a a) (* b b)))

;;Exercise A.4
;;(define (a-plus-abs-b a b)
;; ((if (> b 0) + -) a b))
;; This code takes 2 arguments, a and b.  It goes to the if statement, and
;; adds a and b if b is positive, and subtracts b from a if b is negative.  
;; (so we have a-b if b is negative). Essentially, what it is doing is 
;; taking a and adding the absolute value of b. Note that the "if" returns
;; the operator that we use on variables a and b.


;; Part B: Evaluation

;; Exercise B.1
;; With normal-order evaluation, the result of (test 0 (p)) should be 0.
;; The interpreter should check if x is equal to 0, and since it is, it should
;; evaluate the true case expression of the if statement and return 0, without ever
;; having to look at the false case expression.  However, using applicative-order evaluation
;; will result in an error or an infinite loop, since it will try to evaluate all of the 
;; operands (arguments) first, so it must try to evaluate the y argument which is (p), but (p) is 
;; not correctly defined and is an "illegitiment" value, so the interpreter will be stuck
;; trying to figure out what it is. The function (p) takes no arguments and just returns
;; itself, so it is stuck forever trying to return itself, only to call itself again, so it
;; can never get out of the function call.  In this case, the "lazy evaluation" of the 
;; normal-order evaluation actually helps since we never need to call the function (p)
;; since we go to the true case since x does indeed equal 0.


;; Exercise B.2
;; When Alyssa attempts to use new-if to calculate square roots, her program gets stuck in 
;; and infinite loop.  If she used a regular "if" statement, and the predicate boolean 
;; expression was true, then she could just evaluate the true-case expression and return
;; the first guess that was good enough.  However, by defining her own new-if with conditionals, 
;; she must evaluate both the true-case and false-case expressions, since they are all arguements
;; to the new-if.  This poses a problem, because even when we are "close enough" we still have to 
;; evaluate the false case expression which recursively calls sqrt-iter, which then has to evaluate 
;; the false-case expression again by making anotehr recursive call.  There is no end to evaluating 
;; the false-case expressions, so the program will get stuck as it tries to evaluate the false-case 
;; expression, which is unnecessary if using the built-in "if" statement.

;; Exercise B.3
;; part (a):
;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))

;; Evaluate: (+ 2 5)
;;   evaluate 2 -> 2
;;   evaluate 5 -> 5
;;   evaluate + -> (lambda (a b) (if...))
;;   Apply (lambda (a b)
;;           (if (= a 0)
;;               b
;;               (inc (+ (dec a) b)))) to 2,5
;;     substitute 2 for a, 5 for b in (if...)
;;     evaluate (if (= 2 0)
;;                 5
;;                 (inc (+ (dec 2) 5))))
;;        NOTE: Use if expression's special form to evaluate
;;        evaluate (= 2 0)
;;          evaluate 2 -> 2
;;          evaluate 0 -> 0
;;          evaluate = -> =
;;          apply = to 2,0 -> #f
;;        since expression is false, replace with the false clause:
;;         -> (inc (+ (dec 2) 5)))
;;        evaluate: (inc (+ (dec 2) 5)))
;;         evaluate (+ (dec 2) 5)):
;;           evaluate (dec 2):
;;             evaluate 2 -> 2
;;               apply dec to 2 -> 1
;;           evaluate 5 -> 5
;;           evaluate + -> (lambda (a b) (if...))
;;           Apply (lambda (a b)
;;                   (if (= a 0)
;;                       b
;;                       (inc (+ (dec a) b)))) to 1,5
;;             substitute 1 for a, 5 for b in (if...)
;;                evaluate (if (= 1 0)
;;                             5
;;                             (inc (+ (dec 1) 5))):
;;                  NOTE: Use if expression's special form to evaluate
;;                  evaluate (= 1 0):
;;                    evaluate 1 -> 1
;;                    evaluate 0 -> 0
;;                    evaluate = -> =
;;                    apply = to 1,0 -> #f
;;                  Since expression is false, replace with the false clause:
;;                   -> (inc (+ (dec 1) 5)))
;;                  evaluate (inc (+ (dec 1) 5))):
;;                    evaluate (+ (dec 1) 5)):
;;                      evaluate (dec 1):
;;                        evaluate 1 -> 1
;;                        apply dec to 1 -> 0
;;                      evaluate 5 -> 5
;;                      evaluate + -> (lambda (a b) (if...))
;;                      Apply (lambda (a b)
;;                              (if (= a 0)
;;                                  b
;;                                 (inc (+ (dec a) b)))) to 0,5
;;                        substitute 0 for a, 5 for b in (if...):
;;                          evaluate (if (= 0 0)
;;                                      5
;;                                     (inc (+ (dec 0) 5))):
;;                            NOTE: Use if expression's special form to evaluate
;;                            evaluate (= 0 0):
;;                              evaluate 0 -> 0
;;                              evaluate 0 -> 0
;;                              evaluate = -> =
;;                              apply = to 0,0 -> #t
;;                            Since expression is true, replace with the true clause:
;;                              result: -> 5
;;                              so, we have (+ (dec 1) 5) -> 5
;;                  evaluate (inc 5):
;;                    evaluate 5 -> 5
;;                    apply inc to 5 -> 6
;;        evaluate (inc (+ (dec 2) 5))):
;;          evaluate (+ (dec 2) 5)) -> 6
;;          apply inc to 6 -> 7
;;   Final Result: evaluate (+ 2 5) -> 7

;; part (b):
;; (define (+ a b)
;;  (if (= a 0)
;;      b
;;      (+ (dec a) (inc b))))

;; Evaluate: (+ 2 5)
;;   evaluate 2 -> 2
;;   evaluate 5 -> 5
;;   evaluate + -> (lambda (a b) (if...))
;;   Apply (lambda (a b)
;;           (if (= a 0)
;;               b
;;               (+ (dec a) (inc b)))) to 2,5
;;     substitute 2 for a, 5 for b in (if...)
;;     evaluate (if (= 2 0)
;;                 5
;;                 (+ (dec 2) (inc 5)))
;;        NOTE: Use if expression's special form to evaluate
;;        evaluate (= 2 0)
;;          evaluate 2 -> 2
;;          evaluate 0 -> 0
;;          evaluate = -> =
;;          apply = to 2,0 -> #f
;;        since expression is false, replace with the false clause:
;;         -> (+ (dec 2) (inc 5))
;;        evaluate: (+ (dec 2) (inc 5))
;;           evaluate (dec 2):
;;             evaluate 2 -> 2
;;               apply dec to 2 -> 1
;;           evaluate (inc 5):
;;             evaluate 5 -> 5
;;               apply inc to 5 -> 6
;;           evaluate + -> (lambda (a b) (if...))
;;           Apply (lambda (a b)
;;                   (if (= a 0)
;;                       b
;;                       (+ (dec a) (inc b)))) to 1,6
;;             substitute 1 for a, 6 for b in (if...)
;;                evaluate (if (= 1 0)
;;                             6
;;                             (+ (dec 1) (inc 6))):
;;                  NOTE: Use if expression's special form to evaluate
;;                  evaluate (= 1 0):
;;                    evaluate 1 -> 1
;;                    evaluate 0 -> 0
;;                    evaluate = -> =
;;                    apply = to 1,0 -> #f
;;                  Since expression is false, replace with the false clause:
;;                   -> (+ (dec 1) (inc 6))
;;                  evaluate (+ (dec 1) (inc 6)):
;;                      evaluate (dec 1):
;;                        evaluate 1 -> 1
;;                        apply dec to 1 -> 0
;;                      evaluate (inc 6):
;;                        evaluate 6 -> 6
;;                        apply inc to 6 -> 7
;;                      evaluate + -> (lambda (a b) (if...))
;;                      Apply (lambda (a b)
;;                              (if (= a 0)
;;                                  b
;;                                 (+ (dec a) (inc b)))) to 0,7
;;                        substitute 0 for a, 7 for b in (if...):
;;                          evaluate (if (= 0 0)
;;                                      7
;;                                     (+ (dec 0) (inc 7))):
;;                            NOTE: Use if expression's special form to evaluate
;;                            evaluate (= 0 0):
;;                              evaluate 0 -> 0
;;                              evaluate 0 -> 0
;;                              evaluate = -> =
;;                              apply = to 0,0 -> #t
;;                            Since expression is true, replace with the true clause:
;;                              result: -> 7
;;                              so, we have (+ (dec 1) (inc 6) -> 7
;;                    evaluate (+ (dec 1) (inc 6)) -> 7
;;        evaluate (+ (dec 2) (inc 5))) -> 7
;;   Final Result: evaluate (+ 2 5) -> 7

;; Note that the first (part a) process is linear recursive processes.  The 
;; problem grows linear as a grows, and we need to make more recursive calls 
;; to the function "+".  All of these results need to me stored since we cannot
;; apply the "inc" operator yet, so it is not constant in space.  Rather it
;; grows linearly in space with a, and is therefore a linear recurisve process.

;; However, the second process (part b) is a linear iterative process.  Note 
;; that each time we are just calling the "+" function again with different
;; arguments.  Nothing needs to be stored from the first function call once
;; we call it again with (dec a) and (inc b) as the arguments.  Since we are 
;; just calling the function again (and we don't need to apply any operator 
;; to the result), we don't need more memory - only the state variables are 
;; changing.  Thus, it is a linear iterative process.

;; Part C: Recursion

;; Exercise C.1

;; This function computes the factorial of the input number,
;; which for a number n is equal to n * (n-1) * ... * 1.
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))


;; part (a):
;; This function takes a non-negative interger n and computes the n-th term of the series 
;; expansion of e, which is 1/n!.
(define (e-term n)
  (/ 1 (factorial n)))

;; part (b):
;; This function takes a non-negative integer n and computes and approximation to e
;; by computing the sum of n terms of the infinite series expansion of e.  The function
;; is recrusive and calls itself until it reaches the base case, where the 
;; approximation of e with one term should be 1/0! = 1/1 = 1.
(define (e-approximation n)
  (if (= n 0)
      1.0
      (+ (e-term n)(e-approximation (- n 1)))))

;; part (c):
;; The approximation of e with 100 terms is given by the following:
;;(e-approximation 100) -> 2.7182818284590455


;; Exercise C.2

;; These are two mutually recursive functions that call each other by checking for 
;; equality with n = 0, and if not, calling the other function with n-1.

;; This function takes a single non-negative integer n and returns true if n is even
;; and false otherwise.  It is recursive and makes use of the odd? function.
(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))

;; This function takes a single non-negative integer n and returns true if n is odd
;; and false otherwise.  It is recursive and makes use of the even? function.
(define (odd? n)
  (if (= n 0)
      #f
      (even? (- n 1))))


;; Exercise C.3
;; part(a) Recursive:
;; This function takes an integer n and recursive computes the value of the
;; function f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3), where f(n) = n for n = 0, 1, 2
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3))))))
;; part(b) Iterative:
;; This function takes an integer n and iterative computes the value for the function
;; f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3), where f(n) = n for n= 0, 1, 2.
;; Uses the helper function (f-helper) with state variables.
(define (f-iter n)
  (if (< n 3)
      3
      (f-helper 0 1 2 0 n)))

;; Helper function to reduce f-recursive to an iterative procedure.  Uses 3 state
;; variables and a counter, since we only need to remember the 3 most recent 
;; caluculations of f(n).  This helps avoid calling f(n) many times for the same n.
(define (f-helper f0 f1 f2 current max)
  (if (= current max)
      f0
      (f-helper f1 f2 (+ f2 (* 2 f1) (* 3 f0)) (+ current 1) max)))

;; Exercise C.4
;; This function takes two integer arguments, row and index.  Row is the row of
;; Pascal's triangle, with row 0 corresponding to "1", row 1 = "1 1", etc.
;; Index is the position in the corresponding row, starting from left to right, 
;; with the first position corresponding to 0.  If the given (row, index) pair
;; is not a valid location in the triangle, -1 will be returned to indicate a 
;; failure.
(define (pascal row index)
  (cond ((= index 0) 1)
        ((= index row) 1)
        ((< row index) -1)
        (else (+ (pascal (- row 1) (- index 1)) (pascal (- row 1) index)))))

;; Exercise C.5
(define (geom-sum n)
  (if (= n 0)
      0
      (+ n (geom-sum (/ n 2)))))
;; The problem with Ben Bitdiddle's code is that the base case is never reached.
;; Technically, the base case is true, but we can never get there because we 
;; are attempting to reduce the problem through division by 2, instead of the 
;; usually subtraction.  If n is not originally 0, n/2 will never get to 0 since
;; Scheme can handle arbitrally small rational numbers.  

;; The fundamental design flaw is the handling of the base case, which eventually
;; must be reached so we can recursively work our back up to the original call.
;; But if we can't reach the base case, we'll be stuck in an infinite loop.

;; We could implement something like the following instead, which would use a tolerance
;; for the base case as opposed to an equality to 0, in which case it would
;; eventually be reached by dividing n by 2, and thus the recursive process would 
;; work.  Of course, this is just an approximation for the geometric series, but
;; we can't really do an infinite sum in finite time.

(define (geom-sum2 n)
  (if (< n 0.001)
      0.0
      (+ n (geom-sum2 (/ n 2)))))

      








