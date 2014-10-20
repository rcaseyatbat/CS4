#lang racket
(require htdp/testing)

;; Ryan Casey
;; csman username: rcasey

;; Assignment 6

;; Part A: Mutation

;; Exercise A.1

;; part 1:
;; Yes, Scheme allows using set! to mutate the parameters of a lambda 
;; expression. When the funciton is appplied, a new frame is created that 
;; binds the evaluated arguments of the procedure in the new frame.  This
;; is now the current frame, so set! can work in that fram to mutate
;; the arguments of the lambda.

;; part 2
;; Yes, Scheme allows using set! to mutate bindings inside a let expresion. A 
;; let expression creates a binding in a frame in the enviornment model, so set!
;; can look up the old value of a binding and mutate it inside a let expression.


;; Part B: Message Passing and Mutation

;; Compute the mean of a list of numbers.
;; N.B. length is a built-in Scheme function which computes the
;; length of a list.
(define (mean values)   
  (/ (apply + values)   ;; sum a list of numbers (note clever use of "apply")
     (length values)))  ;; and divide by the length of the list

;; variance: list-of numbers -> number
;; Compute the variance of a list of numbers.
(define (variance values)
  (define (square x) (* x x))
  (- (mean (map square values)) ;; the mean of the squares
     (square (mean values))))   ;; minus the square of the mean

;; make-stat: [nothing] -> message-passing statistics object
;; Creates a statistics object that can be used to calculate mean and 
;; standard deviation of the current list of numbers with 'mean or 'variance.
;; 'add x will add the element x to the current list, while 'clear will empty
;; the current list.
(define (make-stat)
  (let ((items (list)))
  (lambda (op . args)
     (cond ((eq? op 'add)
            (set! items
               (cons (car args) items)))
               ;(+ value (car args))))
           ((eq? op 'mean)
            (if (null? items)
                (error "cannot calculate mean of empty list")
                (mean items)))
           ((eq? op 'variance)
            (if (null? items)
                (error "cannot calculate variance of empty list")
                (variance items)))
           ((eq? op 'clear)
            (set! items
               (list)))
           (else
            (error "unknown op: " op))))))


;; Part C: The Environment Model

;; Exercise C.1
;; A let expression desugars into a lambda expression, so in the environment 
;; model a frame is created that contains all the bindings for the let 
;; expression, and this frame points to the global environment.  When we 
;; actually go to evaluate the let, we go to the frame that was created and
;; evaluate the body of the expression, and then we go back to pointing
;; to the global environment.

;; Exercise C.2
;; SICP 3.9
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
;; See attached drawings: C2.pdf
;; (Recursive Factorial on first page, Iterative Factorial on second page)

;; Exercise C.3
;; SICP 3.11
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)

;; See attached drawing of environment model: C3.pdf

;; The local state for each account is kept in its own distinct frame, so we can
;; set them and keep track of them seperately.  However, the code in 
;; make-account is shared between the accounts, as we can call the same 
;; functions 'withdraw or 'deposit for each account, as the code is shared.


;; Part D: List Mutation
; (define (copy-list x)
;   (if (null? x) 
;       (list)
;       (cons (car x) (copy-list (cdr x)))))

; (define (bash-cdr! x)
;   (if (null? x) 
;       x
;       (begin
;         (set-cdr! (car x) (caar x))
;         (bash-cdr! (cdr x)))))

; (define a (list (cons 1 2) (cons 3 4) (cons 5 6)))
; (define b (copy-list a))
; (bash-cdr! b)
; a 

;; Note that b is a copy of a.  Then, bash-cdr! goes through and sets the value
;; of the second element of each cons pair (through set-cdr!) to be the same as 
;; the first value of the pair, since (caar x) gives the first element.  The 
;; results are all cons-ed together, until we get to the base case in which we 
;; add the empty list and terminate.

;; a-> (cons (cons 1 1) (cons (cons 3 3) (cons (cons 5 5) '())))

;; Exercise D.2
;; SICP 3.13
;; (define (make-cycle x)
;;   (set-cdr! (last-pair x) x)
;;   x)
;; (define z (make-cycle (list 'a 'b 'c)))
;; (last-pair z)

;; where last pair is given as:
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
;; which tries to return the last 'box' of the list.

;; See attached box-and-pointer diagram: D2.pdf

;; Clearly, make-cycle creates a circular list.  Therefore, there is no last 
;; pair in the list, as the 'end' at c just points back to the beginning at a.
;; Therefore, we will enter an infinite loop if we try to compute (last-pair z), 
;; and we will be unable to compute it.

;; Exercise D.3
;; SICP 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
;; See: D3.pdf
;; See attached drawing for box-and-pointer diagrams showing that the above code
;; can produce different results depending on the structure of the list.  
;; Note that the code cannot be correct, as a normal 3-element list should 
;; return 3, but in 3.11 we showed a 3-element list that was circular and thus
;; had infinte pairs.


;; Part E: The Y combinator

(define Y 
  (lambda (f) 
    ((lambda (z) (z z)) 
     (lambda (w) (f (lambda (x) ((w w) x)))))))

;; Exercise E.1

;; sum: list-of-integers -> sum-of-elements
;; Uses the Y combinator to avoid using recursion.
(define sum
   (Y (lambda (f)
         (lambda (items)
            (if (null? (cdr items))
                (car items)
                (+ (car items) (f (cdr items))))))))

(check-expect (sum (list 1 2 3 )) 6)
(check-expect (sum (list 0 1 23 4)) 28)


;; Exercise E.2

;; factorial-interative: n -> n!
;; Takes an integer n and uses the Y-combinator to compute the factorial without
;; making iterative calls.
(define (factorial-iterative n)
  (define factorial-iter
   (Y (lambda (f)
         (lambda (n)
             (if (= (car pair) 0)
                (cdr pair)
                (begin
                  (set! pair (cons (- (car pair) 1) (* (car pair) (cdr pair))))
                   (f pair)))))))
  (define pair (cons n 1))
  (factorial-iter pair)) 


;; writing iterative factorial as a function of 1 argument.
;; (this was used to write the Y combinator for factorial-iter)
(define (factorial-one n)
  (define (fact-iter pair)
    (if (= (car pair) 0)
        (cdr pair)
        (begin
          (set! pair (cons (- (car pair) 1) (* (car pair) (cdr pair))))
          (fact-iter pair))))
  (define pair (cons n 1))
  (fact-iter pair))

(generate-report)