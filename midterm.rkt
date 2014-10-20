#lang racket
(require htdp/testing)

;; Ryan Casey
;; csman username: rcasey

;; Midterm Exam

;; Part 1: Evaluation

;; part (a):
;; (define (f x y) (* (- x 2) (+ x y)))
;; (define (g x) (+ (f x 7) (f (* 3 x) (- x 3))))
;; (g 5)

;; evaluate 5 ==> 5
;; evaluate g ==> (lambda (x) (+ (f x 7) (f (* 3 x) (- x 3)))):
;; apply (lambda (x) (+ (f x 7) (f (* 3 x) (- x 3)))) to 5:
;;   substitute 5 for x into (+ (f x 7) (f (* 3 x) (- x 3))):
;;     ==> (+ (f 5 7) (f (* 3 5) (- 5 3)))
;;       evaluate (f 5 7):
;;         evaluate 5 ==> 5
;;         evaluate 7 ==> 7
;;         evaluate f ==> (lambda (x y) (* (- x 2) (+ x y))):
;;           apply (lambda (x y) (* (- x 2) (+ x y))) to 5, 7:
;;             substitute 5 for x, 7 for y into (* (- x 2) (+ x y)):
;;               ==> (* (- 5 2) (+ 5 7))
;;                evaluate (- 5 2) ==> 3
;;                evaluate (+ 5 7) ==> 12
;;                evaluate (* 3 12) ==> 36
;;           so, we have (f 5 7) ==> 36
;;       evaluate (f (* 3 5) (- 5 3)):
;;         evaluate (* 3 5) ==> 15
;;         evaluate (- 5 3) ==> 2
;;         evaluate f ==> (lambda (x y) (* (- x 2) (+ x y))):
;;           apply (lambda (x y) (* (- x 2) (+ x y))) to 15, 2:
;;             substitute 15 for x, 2 for y into (* (- x 2) (+ x y)):
;;               ==> (* (- 15 2) (+ 15 2))
;;                evaluate (- 15 2) ==> 13
;;                evaluate (+ 15 2) ==> 17
;;                evaluate (* 13 17) ==> 221
;;           so, we have (f (* 3 5) (- 5 3)) ==> 221
;;      evaluate + ==> +
;;      apply + to 36, 221 ==>  257
;; Result: 257

;; part (b)
;; (define (f y z)
;;   (if (= z 0) 
;;       y
;;       (f (* y y) (- z 1))))
;; (f 3 2)

;; evaluate 3 ==> 3
;; evaluate 2 ==> 2
;; evaluate f to (lambda (y z) 
;;                   (if (= z 0) 
;;                      y
;;                     (f (* y y) (- z 1))))
;; apply (lambda (y z)...) to 3, 2:
;; substitute 3 for y, 2 for z in (if (= z 0)... :
;;  ==> (if (= 2 0)
;;         3
;;         (f (* 3 3) (- 2 1)))
;;   evaluate (= 2 0) ==> #f
;;     Since expression is false, replace with the false clause:
;;    ==> (f (* 3 3) (- 2 1))
;;     evaluate (* 3 3) ==> 9
;;     evaluate (- 2 1) ==> 1
;;     evaluate f to (lambda (y z) 
;;                      (if (= z 0) 
;;                       y
;;                      (f (* y y) (- z 1))))
;;     apply (lambda (y z)...) to 9, 1:
;;     substitute 9 for y, 1 for z in (if (= z 0)... :
;;      ==> (if (= 1 0)
;;             9
;;            (f (* 9 9) (- 1 1)))
;;        evaluate (= 1 0) ==> #f
;;          Since expression is false, replace with the false clause:
;;         ==> (f (* 9 9) (- 1 1))
;;          evaluate (* 9 9) ==> 81
;;          evaluate (- 1 1) ==> 0
;;          evaluate f to (lambda (y z) 
;;                           (if (= z 0) 
;;                            y
;;                           (f (* y y) (- z 1))))
;;          apply (lambda (y z)...) to 81, 0:
;;          substitute 81 for y, 0 for z in (if (= z 0)... :
;;           ==> (if (= 0 0)
;;                  81
;;                 (f (* 81 81) (- 0 1)))
;;             evaluate (= 0 0) ==> #t
;;              Since expression is false, replace with the true clause:
;;                ==> 81
;; Result ==> 81

;; part (c):
;; (define (switch a)
;;   (lambda (b)
;;     (if (> a b)
;;         (lambda (a b) (* (+ a b) (- a b)))
;;         *)))
;; (((switch 5) 2) 3 4)

;; evaluate 5 ==> 5
;; evaluate switch ==> (lambda (a) (lambda (b) (if (> a b...
;; apply (lambda (a).....) to 5:
;; substitute 5 for a in (lambda (b) (if (> a b...)
;;  ==> (lambda (b)
;;         (if (> 5 b)
;;            (lambda (a b) (* (+ a b) (- a b))) NOTE: Lambda shielding occurs!
;;            *)))
;;   evaluate 2 ==> 2
;;   apply (lambda (b) (if (> 5 b... to 2:
;;   substitute 2 in for b in (if (> 5 b)...:
;;     ==> (if (> 5 2)
;;            (lambda (a b) (* (+ a b) (- a b)))
;;            *)))
;;       evaluate (> 5 2) ==> #t
;;        Since expression is true, replace with the true clause:
;;         ==> (lambda (a b) (* (+ a b) (- a b)))
;;  evaluate 3 ==> 3
;;  evaluate 4 ==> 4
;;  apply (lambda (a b) (* (+ a b) (- a b))) to 3, 4
;;  substitute 3 for a, 4 for b into (* (+ a b) (- a b)):
;;    ==> (* (+ 3 4) (- 3 4))
;;      evaluate (+ 3 4) ==> 7
;;      evaluate (- 3 4) ==> -1
;;      evaluate * ==> *
;;      apply * to 7, -1:
;;        ==> -7
;; Result: -7


;; Part 2: Time Complexity

;; part (a):
;; Return #t if n is composite (divisible by a number other than 1 and n),
;; or #f if it is not.  Assume that n is a positive integer.
(define (composite? n)
  (define (iter i)
    (cond ((> i (sqrt n)) #f)
          ((= (remainder n i) 0) #t)
          (else (iter (+ i 1)))))
  (iter 2))

;; i. Time Complexity = O(n^(1/2))
;; ii. The time complexity is O(n^(1/2)) because in the worse case scenario, we have
;;    to recursively go through the iter function up to sqrt(n) times, as 
;;    each time we are only increasing i by 1. Assuming that the sqrt and 
;;    remainder functions are O(1) complexity, we go though (iter i) up to
;;    sqrt(n) times if n is not composite, which is the worse case scenario for
;;    the program.  If we assume we spend some constant amount of time inside 
;;    the iter function, that doesn't change the complexity.

;; part (b):
(define (super-fib n)
  (if (< n 3)
      n
      (+ (super-fib (- n 1))
         (super-fib (- n 2))
         (super-fib (- n 3)))))

;; i. Time Complexity = O(3^n)
;; ii. The time complexity is O(3^n) because each call to (super-fib n) makes
;;   3 recursive calls until we hit the base case of n being smaller than 3.
;;   Each recursive call also makes 3 more recursive calls, and we make no 
;;   effort to store the known values through memoization, so we just have to 
;;   keep calculating the same values over and over.
;;   making the approximation that the 3 recursive calls take the same amount
;;   of time:
;;   T(n) = C + 3*T(super-fib n-1)   
;;   T(super-fib n-1) = C + 3*T(super-fib n-2)...etc
;;   This keeps repeating with n decreasing by 1 until we get to the base case, 
;;   each time multiplying the time complexity by 3:
;;   T(n) = 3*3*3*3.....*3  [note that there are ~n 3's]
;;   So, T(n) = 3^n
;;   ==> Time Complexity O(3^n)

;; part (c):
(define (weird n)
  (cond ((< n 1) 1)
        ((= (remainder n 3) 0) (+ 1 (weird (/ n 3))))
        (else (+ 1 (weird (- n 1))))))
;; i. Time Complexity: O(log(n))
;; ii. In the worst case scenario, we start with an n that is not divisible by
;;   2, and we must make 2 calls through weird (- n 1) to find the first factor
;;   of 3. But once we find a factor of 3, we can divide n by 3 in the next call
;;   to weird.  If we were just dividing by 3, it would be O(log(n)) complexity,
;;   where the base of the log is 3, since we are dividing the argument by 3
;;   for each recursive call.  But, we have to add in the fact that we might 
;;   have to make an additional 2 recursive calls each time, since the result of
;;  (/ n 3) might not be divisible by 3.  However, in the asymtotic time 
;;   complexity, this fact doesn't matter.  As n gets really large, the 
;;   important fact is that we can divide n by 3 (in the worse case with 3 
;;   recursive calls).  So, perhaps it should be O(3log(n)), but the constant 
;;   doesn't matter when we deal with large n, so we have O(log(n)).

;; part (d):

;; Find the largest fibonacci number below 1000000.
(define maxval 1000000)

(define (largest-fib)
  (define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))

  (define (fib? n)
    (define (fib?-iter m)
      (cond ((= (fib m) n) #t)
            ((> (fib m) n) #f)
            (else (fib?-iter (+ m 1)))))
    (fib?-iter 0))

  (define (iter n r)
    (cond ((= n maxval) r)
          ((fib? n) (iter (+ n 1) n))
          (else (iter (+ n 1) r))))

  (iter 0 0))

;; i. Time Complexity: O(2^n) where n = 1000000 (asympotically)
;;      - for (fib n): O(2^n)
;;      - for (fib? n): T(n) = (n * fib(n) * fib(n)) ==> O(n * (2^n * 2^n))
;;                                                ==> O(n*2^(n+1)) ==> O(2^n)
;;      - for (iter): T(n) = C + T(fib? n) + T(n+1) ==> O(n*T(fib? n))
;;                        ==> O(n*n*2^(n+1)) ==> O((n^2)*2^(n+1)) ==> O(2^n)
;;      - for (largest fib) = 1000000 * T(iter) ==> O(1000000 * 2^n)
;;                       ==> O(2^n)

;; ii. We essentially have to run though iter 1000000 times, since we are 
;;    increasing the first argument to (+ n 1) each time, starting at 0, and
;;    we only hit the base case when n = 1000000.  Each time throgh, we have to 
;;    call the fib? function.  The fib? function calls (fib m) every time for 
;;    (+ m 1).  We know from class that (fib m) has 2^m time complexity, and in 
;;    the worst case scenario, we have to call it once to check equality and 
;;    once more to check if its greater than m, before calling it again for 
;;    (+ m 1).  So in the worst case, fib? has time complexity m*(2^m).
;;    Now, we have to go through fib for each number n = 1 -> 1000000, where
;;    each number represents m for the fib? call.  So, the time should be the 
;;    sum from n=1 to n=1000000 of (n * (2^n)).  Assymptotically, 2^n >> n, so
;;    we can just call the time complexity O(2^n) where n = 1000000, since 
;;    2^1000000 >>> 2^n for any other n.


;; part (e):
(define (acc p n)
  (define (iter p n result)
    (if (< p 1) 
        result
        (iter (/ p 2) (- n 1) (+ result n))))
  (iter p n 1))

;; i. Time Complexity: O(log(p))
;; ii. The time complexity is O(log(p)) since the base case depends on the value
;;   of p, and we are able to divide p by 2 each time we go though a recursive 
;;   call of iter.  We can only subtract 1 from n each time, but the value of n 
;;   doesn't matter in terminating the program by reaching the base case (the 
;;   value of n is only important in determining the final answer when the base
;;   case is reached).  We have to go though iter a maximum of log(p) (where 
;;   the log is in base 2) since we can divide p by 2 each time through, but
;;   the base doesn't matter, so we have O(log(p)).


;; Part 3: Recursion

;; part (a):
;; fizzbuzz: int -> printed-numbers
;; the printed numbers are 1 -> n, each on a new line, with multiples of 3 
;; replaced by "Fizz", multiples of 5 replaced by "Buzz", and multiples of 
;; 3 AND 5 replaced by "FizzBuzz".
(define (fizzbuzz n)
  (fizz-iter 1 n))

;; fizz-iter: -> int int -> print-numbers-from-int-to-int
;; prints out numbers for counter -> max in same was as (fizzbuzz n), but this
;; time starting at counter instead of 1.
(define (fizz-iter counter max)
  (if (= 0 (remainder counter 3)) (display "Fizz") (display ""))
  (if (= 0 (remainder counter 5)) (display "Buzz") (display ""))
  (if (and (not (= 0 (remainder counter 3))) (not (= 0 (remainder counter 5)))) 
      (display counter)
      (display ""))
  (newline)
  (if (< counter max)
      (fizz-iter (+ counter 1) max)
      (newline))) ;; base case. when we reach the desired n, end the program

;; part (b):
;; split: int list -> list (list-first-n-elements list-rest-elements)
(define (split n items)
  (split-iter (list) items 0 n))

;; split-iter: list list int int -> list (list-first-n-elements list-rest-elements)
;; iterative helper function for (split n items):
(define (split-iter list-one list-two count n)
  (if (= count n) 
      (list (reverse list-one) list-two)
      (if (null? list-two)
          (error "error: split: not enough elements in list")
          (split-iter (cons (car list-two) list-one) (cdr list-two) (+ count 1) n))))
      ;(split-iter (cons (car list-two) list-one) (cdr list-two) (+ count 1) n)))

(check-expect (split 4 '(1 2 3 4 5 6 7 8 9 10)) '((1 2 3 4) (5 6 7 8 9 10)))
(check-expect (split 0 '(1 2 3)) '(() (1 2 3)))
(check-expect (split 1 '(1 2 3)) '((1) (2 3)))
(check-expect (split 2 '(1 2 3)) '((1 2) (3)))
(check-expect (split 3 '(1 2 3)) '((1 2 3) ()))
(check-error (split 4 '(1 2 3)) "error: split: not enough elements in list")

;; part (c):

;; all-cycles: list -> list-of-all-cycles
(define (all-cycles items)
  (cycle-iter items (- (length items) 1) (list)))

;; cycle-iter: list int list -> list-of-all-cycles
;; helper function for cycles
(define (cycle-iter items cycles result)
  (cond ((= cycles 0) (cons (cycle-1 items) result))
        ((< cycles 0) result)
        (else (cycle-iter (cycle-1 items) (- cycles 1) (cons (cycle-1 items) result)))))

;; cycle-1: list -> list-after-one-cycle
;; returns a list that has completed one cycle (moved first element to end of list)
(define (cycle-1 items)
  (if (null? items) (list)
      (if (null? (cdr items)) 
          items
          (append (cdr items) (list (car items))))))

(check-expect (all-cycles '(a b c)) '((a b c) (c a b) (b c a)))
(check-expect (all-cycles '()) '())
(check-expect (all-cycles '(1)) '((1)))
(check-expect (all-cycles '(1 2)) '((1 2) (2 1)))




;; Part 4: Higher-order functions

;; part (a):

;; curry: f(x,y) -> curried-f(x)
(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

;; uncurry: curried-f(x) -> f(x, y)
(define (uncurry f)
  (lambda (x y)
    ((f x) y)))

;; check curry function
(define (add x y) (+ x y))
(define addc (curry add))
(define add1 (addc 1))
(check-expect (add1 42) 43)

;; check uncurry function
(define add2 (uncurry addc))
(check-expect (add2 42 43) 85)


;; part (b):

;; euler-solve-de: f'(x) number number number -> f(x)
;; takes a function of the form g(x, f(x)) = f'(x), a number representing step 
;; size, and a number representing the value of f(0), and a number representing 
;; how many iterations (multiples of dx) we have gone through. Returns the 
;; function f(x), which can be evaluated at any x.
(define (euler-solve-de g dx f0 counter)
  (lambda (x)
    (if (close-enough? x (* dx counter)) 
        ;; if n*dx is close enough to desired value, return function
        (+ f0 (* (g x f0) dx))
        ;; else, call euler-solve-de again, increasing x by dx, and increasing
        ;; the value of f0 appropriate.  Also increase the counter.
        (euler-solve-de (g (+ x dx) (+ f0 (* dx (g x dx)))) dx
                        (+ f0 (* dx (g x dx))) (+ 1 counter)))))
  
;; close-enough?: number number -> boolean
(define (close-enough? guess x)
 (< (abs (- guess x)) 0.001))

;; couldn't figure this one out...

;(define (euler-solve-de1 g dx f0)
;  (lambda (x)
;    (euler-solve-de1 (g (+ x dx) (+ f0 (* dx (g x dx)))) dx + f0 (* dx (g x dx)))))
                
    
(define (g x fx) 
  fx)
(define dx 0.0000001)
(define f0 1.0)
(define f (euler-solve-de g dx f0 0))

;; part (c):

;; iterative-improve: (number -> boolean) (number -> number) -> (number -> number)
(define (iterative-improve good-enough? improve)
   (define (iter-improve guess)
     (if (good-enough? guess)
         guess
         (iter-improve (improve guess))))
   iter-improve)
 
;; sqrt: number -> number
;; only works for positive numbers
(define (sqrt x)
   ((iterative-improve (lambda (guess) ;; implement good-enough? for square root
                         (< (abs (- (square guess) x)) 0.000001)) 
                         ;; note that tolerance = 0.000001
                         ;; returns true if wihtin tolerance, false otherwise
                       (lambda (guess) ;; implement improve for square root
                         (average guess (/ x guess))))
   1.0)) ;; start value, apply iterative-improve to 1.0 (first guess = 1.0)

(check-within (sqrt 0) 0 0.001)
(check-within (sqrt 0.5) .7071 0.0001)
(check-within (sqrt 1) 1 0.0001)
(check-within (sqrt 2) 1.4142156 0.0001)
(check-within (sqrt 4) 2.0 0.0001)
(check-within (sqrt 9) 3.0 0.0001)

;; improve: number number -> number
(define (improve guess x)
   (average guess (/ x guess)))

;; average: number number -> number
(define (average x y)
   (/ (+ x y) 2))

;; good-enough?: number number -> boolean
(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.001))

;; square: number -> number
(define (square x) (* x x))


(generate-report)
      
  
