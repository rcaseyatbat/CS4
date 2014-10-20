#lang racket
(require htdp/testing)

;; Ryan Casey
;; csman username: rcasey

;; Assignment 5

;; Part A: More on quoting

;; Exercise A.1
;; part (a):
;; (let ((x 2)) 
;;   (quote x))
;;  -> 'x
;; since we use quote, we just return 'x instead of x's defined value.

;; part (b):
;; (let ((x 5)) 
;;   (list '(x x) x '(cons '(x x) x)))
;;  -> '((x x) 5 (cons '(x x) x))
;; since '(x x) = (x x), we subsutute 5 in for x, and then we quote the whole
;; last cons term into the new list.

;; part (c):
;; (let ((y 1) 
;;       (z 2) 
;;       (x '(y z))) 
;;   (list x y z))
;;  -> '((y z) 1 2)
;; since x is '(y z), and then we substitute the appropriate values of y and z.

;; part (d):
;; (let  ((x 1)
;;        (y '(2 3))
;;        (z '(4 5 6)))
;;  `(x ,x y ,y ,@y z ,z ,@z))
;;  -> '(x 1 y (2 3) 2 3 z (4 5 6) 4 5 6)
;; since ,* dequotes * to its defined value, and ,@z unquote-splices the value
;; into the list (without the extra parentheses).


;; Exercise A.2
;; part (a):
'(cons a b)

;; part (b):
'(+ '(+ a 3) '(* b 2))

;; Exercise A.3
;; part (a):
;; (+ ''5 5)
;; 2 quotes is not the appropriate way to unquote something.  The operands for 
;; + must be numbers, and ''5 fails to return a number, as it is equivalent to 
;; (quote '5), which creates a list with '5 as the only item in the list, which 
;; cannot be added with +.

;; part (b):
;; (+ '(+ 2 3) 1)
;; Again, + expects to numbers as arguments, and '(+ 2 3) fails to return a
;; number, since it just quotes the whole list, and we get a list of the 3
;; symbols.  It would work if we didn't quote the list, as we would have 
;; (+ 2 3) evaluate to 5 as normal.  But since the whole list is quoted, we 
;; just get the symbol for + and it is never actually applied.


;; Part B: Mutation

;; Exercise B.1
;; part (a):
;; (set! 5 3)
;; The first argument must be a variable to assign a value to.  In this case, 
;; the first argument is 5, which is just a number that cannot be assigned to
;; anything else.

;; part (b):
;; (set! (cons 3 4) (cons 4 5))
;; Again, the first variable is not a variable that can be reassigned.  It is a 
;; given cons pair, which cannot be reassigned with set!.

;; part (c):
;; (define a 42)
;; (set! 'a 10)
;; It would be fine without the quote, but with the quote we are trying to 
;; assign the symbol a to the value 10, instead of the variable a, which is 
;; not what set! does.

;; part (d):
;; (define a 42)
;; (define (symbol) 'a)
;; (set! (symbol) 10)
;; This doesn't work becaue (symbol) evaluates to the symbol a ('a) intstead
;; of the actual variable a.  Then we are trying to use set! on a symbol instad
;; of a variable, but this is not how set! works.


;; Exercise B.2
;; SICP Exercise 3.2
;; make-monitored: (f 'how-many-calls) -> number
;; where the number returned is how many times the function has been called
;; between resets.
(define (make-monitored f)
  (let ((calls 0))
    (define (mf x)
      (cond ((eq? x 'reset-count) (set! calls 0))
            ((eq? x 'how-many-calls?) calls)
            (else (begin (set! calls (+ 1 calls))
                         (f x)))))
    mf))

(define s (make-monitored sqrt))
(s 100)
(check-expect (s 'how-many-calls?) 1)



;; Exercise B.3
;; SICP Exercise 3.8

;; f: a function such that (+ (f 0) (f 1)) returns 0, but (+ (f 1) (f 0))
;; returns 1.  The test demonstrates that Dr. Racket evaluates from left to 
;; right, since we get (+ (f 0) (f 1)) = 0
(define f
  (let ((astate 1)) ;; note that astate persists through each call, so order..
    (lambda (x)        ;;...of evaluation can matter.
      (set! astate (* astate x)) ;; (f 0) sets the state to 0, so that...
                                     ;;...(f 1) is still 0.
      astate))) ;; however, starting with (f 1) sets state to 1...
                  ;;...so sum is not 1 if it is first.
(check-expect (+ (f 0) (f 1))  0)


;; Part C: Tagged Data

;; Scheme code for lab 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding units
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; representing objects
(define (make-meter length)
  (cons 'meter length))
(define (get-tag m) (car m))
(define (get-value m) (cdr m))
(define (meter? m)
  (and (pair? m) (eq? (get-tag m) 'meter)))
(define (make-foot length)
  (cons 'foot length))
(define (foot? m)
  (and (pair? m) (eq? (get-tag m) 'foot)))


;; object specific addition
(define (meter-add a b)
  (make-meter (+ (get-value a) (get-value b))))
(define (foot-add a b)
  (make-foot (+ (get-value a) (get-value b))))


;; conversion between types
(define meters-per-foot 0.3048)
(define (feet-to-meters a)
  (make-meter (* meters-per-foot (get-value a))))

(define feet-per-meter (/ 1 meters-per-foot))
(define (meters-to-feet a)
  (make-foot (* feet-per-meter (get-value a))))

;; revising length-add
(define (length-add a b)
  (cond ((and (meter? a) (meter? b)) 
         (meter-add a b))
        ((and (foot? a) (foot? b)) 
         (foot-add a b))
        ((and (foot? a) (meter? b))
         (meter-add (feet-to-meters a) b))
        ((and (meter? a) (foot? b))
         (meter-add  a (feet-to-meters b)))
        (else (error "length-add given incompatible units:" a b))))

;; examples
(length-add (make-meter 1) (make-meter 1))
(length-add (make-foot 1)  (make-foot 1))
(length-add (make-meter 1) (make-foot 1))

;; more constructors
(define (make-centimeter length)
  (make-meter (/ length 100)))
(define (make-inch length)
  (make-foot (/ length 12)))
(define (make-kilometer length)
  (make-meter (* 1000 length)))

;; smart (type-aware) conversion
(define (get-meters length)
  (cond ((meter? length) length)
        ((foot? length) (feet-to-meters length))
        (else 
         (error "get-meters requires a length, but given" length))))
(define (get-feet length)
  (cond ((foot? length) length)
        ((meter? length) (meters-to-feet length))
        (else 
         (error "get-feet requires a length, but given" length))))

;; examples
(get-meters (make-meter 1))
(get-meters (make-foot 3))
;;(get-meters 4)

;; another add
;; (define (length-add a b)
;;   (meter-add (get-meter a) (get-meter b)))
;; what does this return when adding feet?

;; other types / units
;(define (make-gram mass)   (cons 'gram mass))
(define (make-second time) (cons 'second time))
(define minutes-per-second 60)
(define (make-minute time) 
  (make-second 
   (* time minutes-per-second)))
;(define (gram? m)
 ; (and (pair? m) (eq? 'gram (get-tag m))))
(define (second? m)
  (and (pair? m) (eq? 'second (get-tag m))))

;; detecting types
(define (length? a)
  (or (meter? a) (foot? a)))
;(define (mass? a)
 ; (gram? a))
(define (time? a)
  (second? a))

(define (second-add a b)
  (make-second (+ (get-value a) (get-value b))))

(define (time-add a b)
  (cond ((and (second? a) (second? b))
         (second-add a b))
        (else 
         (error "incompatible types:" a b))))

;; unit addition
(define (unit-add a b)
  (cond ((and (length? a) (length? b))
         (length-add a b))
        ((and (time? a) (time? b))
         (time-add a b))
        ((and (mass? a) (mass? b))
         (mass-add a b))
        (else 
         (error "incompatible units:" a b))))

;; examples
(unit-add (make-meter 1) (make-foot 1))
(unit-add (make-second 1) (make-minute 2))
;;(unit-add (make-second 3) (make-foot 1))

;; Exercise C.1
;; part (a):
(define (make-gram mass)  
  (cons 'gram mass))
(define (make-slug mass)  
  (cons 'slug mass))
;; part (b):
(define (gram? m)
  (and (pair? m) (eq? 'gram (get-tag m))))
(define (slug? m)
  (and (pair? m) (eq? 'slug (get-tag m))))
;; part (c):
(define (gram-add a b)
  (make-gram (+ (get-value a) (get-value b))))
(define (slug-add a b)
  (make-slug (+ (get-value a) (get-value b))))
(check-expect (get-value (gram-add (make-gram 1) (make-gram 1))) 2)
(check-expect (get-value (slug-add (make-slug 21) (make-slug 3))) 24)
;; part (d):
(define grams-per-slug 14593.903203)
(define (slug-to-grams a)
  (make-gram (* grams-per-slug (get-value a))))

(define slugs-per-gram (/ 1 grams-per-slug))
(define (grams-to-slug a)
  (make-slug (* slugs-per-gram (get-value a))))

(define (get-gram mass)
  (cond ((gram? mass) mass)
        ((slug? mass) (slug-to-grams mass))
        (else 
         (error "get-meters requires a length, but given" mass))))
(define (get-slug mass)
  (cond ((slug? mass) mass)
        ((gram? mass) (grams-to-slug mass))
        (else 
         (error "get-feet requires a length, but given" mass))))

(define g1
  (make-gram 1))
(define s1
  (make-slug 1))
(check-expect (get-value (get-gram g1)) 1)
(check-within (get-value (get-gram s1)) 14593.903203 .01)
(check-expect (get-value (get-slug s1)) 1)
(check-within (get-value (get-slug g1)) 6.852176460882889e-05 .01)

;; part (e):
(define (mass-add a b)
  (cond ((and (gram? a) (gram? b)) 
         (gram-add a b))
        ((and (slug? a) (slug? b)) 
         (slug-add a b))
        ((and (slug? a) (gram? b))
         (gram-add (slug-to-grams a) b))
        ((and (gram? a) (slug? b))
         (gram-add  a (slug-to-grams b)))
        (else (error "length-add given incompatible units:" a b))))
(check-expect (get-value (mass-add g1 g1)) 2)
(check-expect (get-value (mass-add s1 s1)) 2)
(check-within (get-value (mass-add g1 s1)) 14594.903203 .01)
(check-within (get-value (mass-add s1 g1)) 14594.903203 .01)
;; note that this implementation always returns grams if given one slug and 
;; one gram to add.

;; part (f):
(define (mass? a)
  (or (gram? a) (slug? a)))
(check-expect (mass? g1) #t)
(check-expect (mass? s1) #t)
(check-expect (mass? 42) #f)


;; Exercise C.2
;; With the additions from C.1, unit-add can now handle more additions:
;; part (a):
;; (unit-add (make-gram 20) (make-gram 23))
;;     --> '(gram . 43)
;; part (b):
;; (unit-add (make-gram 2000) (make-slug 1))
;;     --> '(gram . 16593.903203)
;; part (c):
;; (unit-add (make-slug 3) (make-slug 1))
;;     --> '(slug . 4)
;; part (d):
;; (unit-add (make-slug 3) (make-meter 1)) ;; should give an error
;;     --> incompatible units: (slug . 3) (meter . 1)


;; Part D: Message Passing

;; Exercise D.1

(define (gram x)
   (lambda (op . args)
     (cond
       ((eq? op 'get-slug) (get-slug x))
       ((eq? op 'get-gram) (get-gram x))
       ((eq? op 'unit-type) 'gram)
       ((eq? op 'compatible) ;(mass? x))
        (and (mass? x) (mass? (car args))))
       ;((eq? op 'add) (mass? x))
       ((eq? op 'add)
        (make-gram
         (+ x ((car args) 'get-gram))))
       (else (error "unknown op" op)))))

;(define foo (gram 3))
(define foo (gram g1)) ;; Note g1 defined earlier as '(gram . 1)
(foo 'unit-type)
(foo 'get-gram)
(foo 'get-slug)
;(foo 'compatible)
(define agram (make-gram 1))
(define bslug (make-slug 1)) 
(define cfoot (make-foot 1))


;; Exercise D.2

;; make-number: number -> mp-number
;; Define a number as a message-passing object. 
;; "mp-number" in the contract means "message-passing number".
;; "value" is just an ordinary Scheme number.
(define (make-number value)
  (lambda (m . args) ;; remember dotted tail notation!
    (cond ((eq? m 'derive) (make-number 0))  ; derivative of a number is 0
          ((eq? m 'print) value)
          ((eq? m 'evaluate) 
           (make-number value)) ; must evaluate to a message-passing object
          ((eq? m 'zero?) (= value 0))
          ((eq? m 'number?) #t)
          ((eq? m 'value) value)
          (else (error "unknown message: " m)))))

;; make-variable: symbol -> mp-variable
;; Define a variable as a message-passing object.
;; "mp-variable" in the contract means "message-passing variable".
;; "varname" is a Scheme symbol.
(define (make-variable varname)         
  (lambda (m . args)
    (cond ((eq? m 'derive) 
           (if (and (= (length args) 1) ;; length returns the length of a list
                    (symbol? (car args)))
               (if (eq? (car args) varname)
                   (make-number 1)
                   (make-number 0))
               (error "derive needs a variable argument")))
          ((eq? m 'print) varname)
          ((eq? m 'zero?) #f)
          ((eq? m 'number?) #f)
          ((eq? m 'value) 
           (error "should not be asking for the value of a variable"))
          ((eq? m 'evaluate) 
           (if (and (= (length args) 2)
                    (symbol? (car args))
                    (number? (cadr args)))  ;; n.b. (cadr args) is same as (car (cdr args))
               (if (eq? varname (car args)) 
                   (make-number (cadr args))
                   (make-variable varname))
               (error "evaluate needs a variable symbol and a number")))
          (else (error "unknown message: " m)))))

;; make-sum: mp-expr mp-expr -> mp-expr
;; Define a sum as a message-passing object.
;; "mp-expr" in the contract means "message-passing expression",
;; i.e. a message-passing object representing an algebraic expression.
(define (make-sum exp1 exp2)
  (cond ((exp1 'zero?) exp2) ;; exp + 0 --> exp
        ((exp2 'zero?) exp1)
        ((and (exp1 'number?) (exp2 'number?))
         (make-number (+ (exp1 'value) (exp2 'value)))) ;; num + num --> num
        (else  ;; create a new message-passing object representing the sum
         (lambda (m . args)
           (cond ((eq? m 'derive) 
                  (if (and (= (length args) 1)
                           (symbol? (car args)))
                      (let ((variable (car args)))
                        ;; derivative of a sum is the sum of the derivatives
                        ;; of the parts of the sum
                        (make-sum (exp1 'derive variable) 
                                  (exp2 'derive variable)))
                      (error "derive needs a variable argument")))
                 ((eq? m 'print) (list '+ (exp1 'print) (exp2 'print)))
                 ((eq? m 'zero?) #f)
                 ((eq? m 'number?) #f)
                 ((eq? m 'value) 
                  (error "should not be asking for the value of a sum expression"))
                 ((eq? m 'evaluate) 
                  (if (and (= (length args) 2)
                           (symbol? (car args))
                           (number? (cadr args)))
                      (let ((variable (car args))
                            (number   (cadr args)))
                        (let ((exp1-eval (exp1 'evaluate variable number))
                              (exp2-eval (exp2 'evaluate variable number)))
                          (make-sum exp1-eval exp2-eval)))
                      (error "evaluate needs a variable symbol and a number")))
                 (else (error "unknown message: " m)))))))

;; evaluate: mp-expr symbol number -> mp-expr
;; Evaluate a message-passing expression with a number
;; substituted for a variable.
(define (evaluate expression variable value)
  (expression 'evaluate variable value))

;; print: mp-expr -> (list-of symbols OR symbol OR number)
;; Return the expression as its representation as a Scheme list,
;; or as a symbol or a number if possible.
(define (print expression)
  (expression 'print))

;; We ask you to define differentiate below.


;; part 1:

;; make-product: mp-expr mp-expr -> mp-expr
;; Define a product as a message-passing object.
;; "mp-expr" in the contract means "message-passing expression",
;; i.e. a message-passing object representing an algebraic expression.
(define (make-product exp1 exp2)
  (cond ((exp1 'zero?) 0) ;; exp * 0 --> 0
        ((exp2 'zero?) 0)
        ((and (exp1 'number?) (= (exp1 'value) 1)) exp2) ;; exp * 1 -> exp
        ((and (exp2 'number?) (= (exp2 'value) 1)) exp1)
        ((and (exp1 'number?) (exp2 'number?))
         (make-number (* (exp1 'value) (exp2 'value)))) ;; num * num --> num
        (else  ;; create a new message-passing object representing the product
         (lambda (m . args)
           (cond ((eq? m 'derive) 
                  (if (and (= (length args) 1)
                           (symbol? (car args)))
                      (let ((variable (car args)))
                        ;; derivative of a product by the product rule
                        ;; Deriv(f*g) = g*Deriv(f(x)) + f * Deriv(g(x))
                        (make-sum (make-product exp1 (exp2 'derive variable)) 
                                  (make-product exp2 (exp1 'derive variable))))
                      (error "derive needs a variable argument")))
                 ((eq? m 'print) (list '* (exp1 'print) (exp2 'print)))
                 ((eq? m 'zero?) #f)
                 ((eq? m 'number?) #f)
                 ((eq? m 'value) 
                  (error "should not be asking for the value of a product expression"))
                 ((eq? m 'evaluate) 
                  (if (and (= (length args) 2)
                           (symbol? (car args))
                           (number? (cadr args)))
                      (let ((variable (car args))
                            (number   (cadr args)))
                        (let ((exp1-eval (exp1 'evaluate variable number))
                              (exp2-eval (exp2 'evaluate variable number)))
                          (make-product exp1-eval exp2-eval)))
                      (error "evaluate needs a variable symbol and a number")))
                 (else (error "unknown message: " m)))))))

;; part 2:
;; differentiate: mp-expr symbol -> mp-expr
;; Returns the derivative of a message-passing expression with respect to the
;; given variable.
(define (differentiate expression variable)
  (expression 'derive variable))

;; part 3:
;; part (a):
(define f1
  (make-sum
   (make-product 
    (make-variable 'x)
    (make-product 
     (make-variable 'x)
     (make-product
      (make-variable 'x)
      (make-variable 'y))))
   (make-sum
    (make-product 
     (make-number 3)
     (make-product
      (make-variable 'x)
      (make-product
       (make-variable 'x)
       (make-product
        (make-variable 'y)
        (make-variable 'y)))))
    (make-sum
     (make-product 
      (make-variable 'y)
      (make-variable 'y))
     (make-number 2)))))
(f1 'print)
;; --> '(+ (* x (* x (* x y))) (+ (* 3 (* x (* x (* y y)))) (+ (* y y) 2)))

;; part (b):
;; Couldn't quite get make-product to work...but the result would look 
;; something like this:
;; (define dfdx (differentiate f1 'x))
;; (dfdx 'print)
;; --> '(+ (* 3 (* x (* x y))) (+ (* 3 (* 2 (* x (* y y)))))


;; part (c):
(print (evaluate f1 'x 3))
;; -> '(+ (* 3 (* 3 (* 3 y))) (+ (* 3 (* 3 (* 3 (* y y)))) (+ (* y y) 2)))

;; part (d):
(print (evaluate (evaluate f1 'x 3) 'y 4))
;; -> 558

;; part (e):
;; Again, couldn't quite get derivative to work. I feel like I'm pretty close..
;; but we would have:
;; (print (evaluate (evaluate dfdx 'x 3) 'y 4))
;; --> 396



(generate-report)