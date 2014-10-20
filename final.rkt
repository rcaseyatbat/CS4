#lang racket

;; ---------------------
;; CS 4 final exam code.
;; ---------------------

;;
;; Rules for jumping:
;; -- no color can jump over an empty square
;; -- any color can jump over a square with the same color, clearing the square
;; -- any primary color can jump over a square with another (possibly different) 
;;    primary color, clearing the square
;; -- a primary color can jump over a secondary color containing that primary color,
;;    removing that color from the secondary color to make a primary color
;;
;; Rules for landing:
;; -- any color can land on an empty square, which gets that color
;; -- any color can land on a square of its own color, which is unchanged
;; -- a primary color can land on a square with a (possibly different) primary color;
;;    the square gets the merge of the two primary colors
;;

;;
;; Part 0: External helper functions supplied to the students.
;;

;; This is useful when writing incomplete versions of code, so that the
;; code can actually run.
(define (unimplemented) (error "unimplemented"))

(define (print s)
  (display s)
  (newline))

;; Return #t if x is in the list lst.
(define (member? x lst)
  (list? (memq x lst)))

;; Choose a random element from a list.
(define (random-element list)
  (list-ref list (random (length list))))

;; Choose a random color.
(define (random-color)
  (random-element '(r y b o g p)))

;; Return #t if the argument is a valid color symbol.
(define (is-valid-color? c)
  (member? c '(r y b o g p e)))

;; Return #t if the argument is a valid primary color symbol.
(define (is-primary-color? c)
  (member? c '(r y b)))

;; Return #t if the argument is a valid secondary color symbol.
(define (is-secondary-color? c)
  (member? c '(g o p)))

;; Return #t if the argument is a valid direction symbol.
(define (is-valid-direction? d)
  (member? d '(n s e w ne nw se sw)))

;; Return #t if the argument n is a valid integer between 0 and highest
(define (is-valid-number? n highest)
  (and (integer? n) (>= n 0) (<= n highest)))


;; Check that the number of arguments for a message is correct.
;; Example: (check-args args 1 "make-square 'jump-ok?")
(define (check-args args n label)
  (if (not (= (length args) n))
      (error (string-append label ": incorrect number of arguments"))
      (void)))

;; returns a list containing the following:
;; first element: whether red is in the color
;; second element: whether yellow is in the color
;; third element: whether blue is in the color
(define (color-representation color)
  (cond ((eq? color 'e) (list #f #f #f))
        ((eq? color 'r) (list #t #f #f))
        ((eq? color 'y) (list #f #t #f))
        ((eq? color 'b) (list #f #f #t))
        ((eq? color 'o) (list #t #t #f))
        ((eq? color 'p) (list #t #f #t))
        ((eq? color 'g) (list #f #t #t))))

;; converts a 3-element list to a symbol for a color where:
;; first element: whether red is in the color
;; second element: whether yellow is in the color
;; third element: whether blue is in the color
(define (color-symbol representation)
  (cond ((eq? representation (list #f #f #f)) 'e)
        ((eq? representation (list #t #f #f)) 'r)
        ((eq? representation (list #f #t #f)) 'y)
        ((eq? representation (list #f #f #t)) 'b)
        ((eq? representation (list #t #t #f)) 'o)
        ((eq? representation (list #t #f #t)) 'p)
        ((eq? representation (list #f #t #t)) 'g)))
  

;;
;; Part 1: Square object implementation.  Worth 8 points.
;;

(define (make-square)
  ;; Helper functions can go here.
  
  (define (contains myColor)
    (cond ((eq? myColor 'r) (list 'r))
          ((eq? myColor 'y) (list 'y))
          ((eq? myColor 'b) (list 'b))
          ((eq? myColor 'o) (list 'r 'y))
          ((eq? myColor 'g) (list 'b 'y))
          ((eq? myColor 'p) (list 'r 'b))))
  
  ;; returns true if colorJumper can legally jump over colorOver
  ;; returns false otherwise
  (define (jump-help colorOver colorJumper)
    (cond ((eq? colorOver 'e) #f)
          ((eq? colorOver colorJumper) #t)
          ((is-primary-color? colorOver) (is-primary-color? colorJumper))
          ((is-secondary-color? colorOver) 
           (member? colorJumper (contains colorOver)))))
  
  
  ;; returns true if colorLanding can legally land on colorOn
  ;; returns false otherwise
  (define (land-help colorOn colorLanding)
    (cond ((eq? colorOn 'e) #t)
          ((is-secondary-color? colorOn)
           (if (eq? colorOn colorLanding)
               #t
               #f))
          ((is-primary-color? colorOn)
           (is-primary-color? colorLanding))))
  
  ;; Stored data.
  ;; initialize the color of the square to be random
  (let ((color (random-color)))   
    
    ;; Helper functions can go here too.
    
    ;; sets new color to be the primary color after the color is removed from 
    ;; a secondary color
    (define (remove primary secondary)
      (cond ((eq? secondary 'o)
             (if (eq? primary 'r)
                 (set! color 'y)
                 (if (eq? primary 'y)
                     (set! color 'r)
                     (error "can't jump over orange with blue"))))
            ((eq? secondary 'p)
             (if (eq? primary 'r)
                 (set! color 'b)
                 (if (eq? primary 'b)
                     (set! color 'r)
                     (error "can't jump over purple with yellow"))))
            ((eq? secondary 'g)
             (if (eq? primary 'y)
                 (set! color 'b)
                 (if (eq? primary 'b)
                     (set! color 'y)
                     (error "can't jump over green with red"))))))
    
    ;; sets the new color of the square to be the combination of the 2 primary
    ;; colors "mixing" together on that square
    (define (add primary1 primary2)
      (cond ((eq? primary1 'r)
             (if (eq? primary2 'r)
                 (void)
                 (if (eq? primary2 'b)
                     (set! color 'p)
                     (set! color 'o))))
            ((eq? primary1 'b)
             (if (eq? primary2 'b)
                 (void)
                 (if (eq? primary2 'r)
                     (set! color 'p)
                     (set! color 'g))))
            ((eq? primary1 'y)
             (if (eq? primary2 'y)
                 (void)
                 (if (eq? primary2 'b)
                     (set! color 'g)
                     (set! color 'o))))))
                
    ;; sets new color of square after a jump over by colorJumper
    (define (do-jump! colorOver colorJumper)
      (cond ((eq? colorOver colorJumper) (set! color 'e))
          ((and (is-primary-color? colorOver) (is-primary-color? colorJumper))
             (set! color 'e))
          ((and (is-secondary-color? colorOver) (is-primary-color? colorJumper))
           (remove colorJumper colorOver))))
    
    ;; sets new color of square after a land on by colorLanding
    (define (do-land! colorOnto colorLanding)
      (cond ((eq? colorOnto 'e) (set! color colorLanding))
            ((eq? colorOnto colorLanding) (void))
          ((and (is-primary-color? colorOnto) (is-primary-color? colorLanding))
             (add colorOnto colorLanding))))
    
    ;; The message-passing object.
    (lambda (op . args)
      (cond ((eq? op 'debug) 
             (check-args args 0 "make-square 'debug")
             (print (color-representation color)))
            ((eq? op 'randomize) 
             (check-args args 0 "make-square 'randomize")
              (set! color (random-color)))
            ((eq? op 'color)
             (check-args args 0 "make-square 'color")
              color)
            ((eq? op 'clear) 
             (check-args args 0 "make-square 'clear")
              (set! color 'e))
            ((eq? op 'jump-ok?)
             (check-args args 1 "make-square 'jump-ok?")
              (if (is-valid-color? (car args))
                  (jump-help color (car args)) ;; #t if valid jump over
                  (error "not a valid color: " (car args))))
            ((eq? op 'land-ok?)
             (check-args args 1 "make-square 'land-ok?")
              (if (is-valid-color? (car args))
                  (land-help color (car args)) ;; #t if valid land on
                  (error "not a valid color: " (car args))))
            ((eq? op 'jump!)
             (check-args args 1 "make-square 'jump!")
              (if (is-valid-color? (car args))
                  (if (jump-help color (car args))
                      (do-jump! color (car args)) ; call to helper fucntion
                      (error "not a valid jump: " (car args)))
                  (error "not a valid color: " (car args))))
            ((eq? op 'land!)
             (check-args args 1 "make-square 'land!")
              (if (is-valid-color? (car args))
                  (if (land-help color (car args))
                      (do-land! color (car args)) ; call to helper function
                      (error "not a valid land operation: " (car args)))
                  (error "not a valid color: " (car args))))
            (else (error "unknown operation: " op))))))

;; The internal representation is hardly used at all (except for 'debug). It is 
;; the cleanest possible code, as I essentially ran through all of the color
;; possibilities as opposed to using the internal representation of the 
;; 3-element list (which it might have been easier to modify).  But this way, 
;; changing the internal representation would only mean changing the helper 
;; functions that convert a color symbol to a internal representation and 
;; vice versa, which would not be much work at all.



;;
;; Part 2: Board object implementation.  Worth 7 points.
;;

(define (make-board nrows ncols)
  ;; Helper functions can go here.
  
  ;; helper function to build a vector of the right number of rows
  ;; returns a vector of vectors representing the game board
  (define (init rows cols)
    (let ((board (make-vector rows)))
      (for ((i 0) (< i rows) (+ i 1))
        (vector-set! board i (make-row cols)))
      board))
      
  
  ;; helper function to make a single row given the width (# of columns)
  (define (make-row cols)
    (build-vector cols g))
  
  ;; macro for a "for loop" used to build the board
  (define-syntax for
  (syntax-rules ()  ;; no keywords
    ((for ((var init) test incr) command ...)
     (do ((var init incr))
          ((not test) (void))
       command ...))))
  
  ;; helper function to get a square at a specific location
  (define (get-square board row col)
      (if (and (is-valid-number? row (- nrows 1)) 
               (is-valid-number? col (- ncols 1)))
         (vector-ref (vector-ref board row) col)
         (void)));(error "not a valid square")))
  
  ;; helper function to make a square for each input
  (define (g x)
    (make-square))
  
  ;; returns the square x units away from rol/col in the specified direction
  (define (x-away board row col direction x)
    (cond ((eq? direction 's) (get-square board (+ row x) col))
          ((eq? direction 'n) (get-square board (- row x) col))
          ((eq? direction 'w) (get-square board row (- col x)))
          ((eq? direction 'e) (get-square board row (+ col x)))
          ((eq? direction 'nw) (get-square board (- row x) (- col x)))
          ((eq? direction 'ne) (get-square board (- row x) (+ col x)))
          ((eq? direction 'sw) (get-square board (+ row x) (- col x)))
          ((eq? direction 'se) (get-square board (+ row x) (+ col x)))
          (else
           (error "not a valid move"))))
  
    
  ;; gets square 1 space away in specified direction
  (define (move1 board row col direction)
    (x-away board row col direction 1))
  
  ;; gets square 2 spaces away in specified direction
  (define (move2 board row col direction)
    (x-away board row col direction 2))
  
    
  
  (define (valid-jump? board row col direction)
    (let ((start (get-square board row col))
          (over (move1 board row col direction))
          (land (move2 board row col direction)))
      (if (and (not (void? start)) (not (void? over)) (not (void? land)))
          (and (over 'jump-ok? (start 'color)) (land 'land-ok? (start 'color)))
          #f)))
  
  (define (do-jump! board row col direction)
    (let ((start (get-square board row col))
          (over (move1 board row col direction))
          (land (move2 board row col direction)))
      (begin
        (over 'jump! (start 'color))
        (land 'land! (start 'color))
        (start 'clear))))
  
  
  ;; This function is supplied to the students:
  ;; prints the current state of the board
  (define (print-board board)
    (define (print-columns i)
      (if (= i ncols)
          (void)
          (begin
            (display i)
            (display " ")
            (print-columns (+ i 1)))))
    (define (iter r c)
      (cond ((= r nrows) (void))
            ((= c ncols)
             (newline)
             (iter (+ r 1) 0))
            (else
             (if (= c 0)
                 (begin
                   (display r)
                   (display " | "))
                 (display " "))
             (let ((color ((get-square board r c) 'color)))
               (if (eq? color 'e) ;; empty square
                   (display ".")
                   (display color)))
             (iter r (+ c 1)))))
    (newline)
    (display "    ")
    (print-columns 0)
    (newline)
    (print (string-append "  +" (make-string (* ncols 2) #\-)))
    (iter 0 0)
    (newline))
  
  ;; The message-passing object itself.
  (if (or (<= nrows 0) (<= ncols 0))
      (error "make-board: invalid dimensions: " nrows ncols)
      (let ((board (init nrows ncols)))
        (lambda (op . args)
          (cond ((eq? op 'reset!) 
                 (check-args args 0 "make-board 'reset")
                 (set! board (init nrows ncols)))
                ((eq? op 'can-jump?)
                 (check-args args 3 "make-board 'can-jump?")
                 (if (and (is-valid-number? (car args) (- nrows 1))
                          (is-valid-number? (car (cdr args)) (- ncols 1))
                          (is-valid-direction? (car (cdr (cdr args)))))
                     (valid-jump? board (car args) (car (cdr args)) 
                                  (car (cdr (cdr args))))
                     #f))
                ((eq? op 'jump!)
                 (check-args args 3 "make-board 'jump!")
                 (if (and (is-valid-number? (car args) (- nrows 1))
                          (is-valid-number? (car (cdr args)) (- ncols 1))
                          (is-valid-direction? (car (cdr (cdr args)))))
                     (do-jump! board (car args) (car (cdr args)) 
                                  (car (cdr (cdr args))))
                     (error "invalid argument to 'jump!")))
                ((eq? op 'print)
                 (check-args args 0 "make-board 'print")
                 (print-board board))
                (else (error "unknown operation: " op)))))))

;;
;; Part 3: Game object implementation.  Supplied to the students.
;;

(define (make-game nrows ncols)
  (define (valid-read? lst)
    (if (and (list? lst) (= (length lst) 3))
        (let ((row (car lst))
              (col (cadr lst))
              (dir (caddr lst)))
          (if (and (integer? row) (integer? col) (symbol? dir))
              (and (in-range row col) (is-valid-direction? dir))
              #f))
        #f))
  
  (define (interact! board)
    (board 'print)
    (display "Enter a (row column direction): ")
    (let ((vals (read)))
      (cond ((eq? vals 'quit) (void))
            ((eq? vals 'reset) 
             (board 'reset!)
             (interact! board))
            ((not (valid-read? vals))
             (print "Invalid input; please try again!")
             (interact! board))
            (else
             (let ((row (car vals))
                   (col (cadr vals))
                   (dir (caddr vals)))
               (if (not (board 'can-jump? row col dir))
                   (print "Invalid jump; please try again!")
                   (board 'jump! row col dir))
               (interact! board))))))
  
  (let ((board (make-board nrows ncols)))
    (interact! board)))

;; The abstraction layer makes it so that we only access the board through 
;; get-square, so if we changed the board representation, we would only have
;; to change that one function to be able to get squares correctly.  All 
;; other functions rely on this function, so making that one correct will
;; be less work than rewriting all of the other code.  Some of the other 
;; helper functions also rely on the board, but ultimately come down to 
;; using get-square, so if we fix that function, all would be good.

(make-game 5 6)

(define square (make-square))