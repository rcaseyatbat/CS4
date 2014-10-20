#lang racket
(require htdp/testing)

;; Ryan Casey
;; csman username: rcasey

;; Assignment 7

;; Part A: Miniproject: Lights Out Game

;; Exercise A.1

;; creates a light object for Light's Out!
(define (make-light state)
  (let ((neighbors (list)))
  (lambda (op . args)
     (cond ((eq? op 'add-neighbor!)
            (if (not (null? (car args)))
              (if (eq? ((car args) 'get-type) 'light)
                  (set! neighbors
                        (cons (car args) neighbors))
                (error "unknown state: " (car args)))
              (display ""))) ; do nothing for invalid add-neighbor call
           ((eq? op 'get-type) 'light)
           ((eq? op 'get-state) state)
           ((eq? op 'set-state!) 
            (if (or (eq? (car args) 'on) (eq? (car args) 'off))
                (set! state (car args))
                (error "unknown state: " (car args))))
           ((eq? op 'toggle!)
            (if (eq? state 'on)
                (set! state 'off) 
                (set! state 'on)))
           ((eq? op 'update!)
            (let ((f (lambda (x) (x 'toggle!))))
              (for-each f neighbors))) ;; note: we will add the light we are 
                                      ;; applying 'toggle to the neighbor list.
           (else
            (error "unknown op: " op))))))

;; returns the nth element of the input list (starting index at 0)
;; an extra useful quick function (to be used to get a particular light in part b)
(define nth-in-list 
     (lambda (items n)
      (if (eq? n 0)
          ; this is the trivial case
          (if (null? items)
              (list)
              (car items))
          ; otherwise find the result in the tail of ls
          (if (null? items)
              (list)
              (nth-in-list (cdr items) (- n 1))))))

  
  
;; Exericse A.2  
  
  ;; creates the game object for Light's Out!
  (define (make-game)
  ;;
  ;; Board representation:
  ;;
  (let ((board (list))) ;; initialize the board variable to an empty list
    
  (define (make-board) ;; makes the board the right size (called in init!)
    (let ((board (list))
          (row (list (list) (list) (list) (list) (list))))
      ;; the board is now a list of 5 (empty) lists each
    (set! board (list row row row row row))
    board))
  
    ;; returns the light at the specified row and column
  (define (get-light board row col)
    (if (and (<= 0 row) (>= 4 row))
        (if (and (<= 0 col) (>= 4 col))
            (nth-in-list (nth-in-list board row) col)
            (error "invalid column: " col))
            (error "invalid row: " row)))
  
    ;; sets the light at the specified row and column to the specified state
  (define (set-light! board row col state)
    (if (and (<= 0 row) (>= 4 row) (and (<= 0 col) (>= 4 col)))
        (if (or (eq? state 'on) (eq? state 'off))
            ((get-light board row col) 'set-state! state)
            (error "invalid state: " state))
        (error "invalid row/col pair: " row col)))
  
  ;;
  ;; Helper functions:
  ;;
 
  ;; Initialize the board given a list of lists of on/off values 
  ;; by setting the corresponding lights to those values.
  (define (initialize! board init)
    (let ((row1 (make-lights (car init))) ; make the 5 rows
           (row2 (make-lights (car (cdr init))))
           (row3 (make-lights (car (cdr (cdr init)))))
           (row4 (make-lights (car (cdr (cdr (cdr init))))))
           (row5 (make-lights (car (cdr (cdr (cdr (cdr init))))))))
      (list row1 row2 row3 row4 row5))) ; combine the 5 rows together to make new board
    
   ;; returns a list of 5 new lights from 'on/'off input.
  (define (make-lights items)
    (list (g (car items)) (g (car (cdr items))) (g (car (cdr (cdr items)))) 
          (g (car (cdr (cdr (cdr items))))) 
          (g (car (cdr (cdr (cdr (cdr items))))))))
   
  ;; function to create a light for one specific element of a list
  (define (g item)
    (if (eq? item 'on)
        (make-light 'on)
        (make-light 'off)))
              
  
  ;; Connect all orthogonally adjacent lights to each other.
  (define (connect! board)
    (define (iter row col)
      (cond ((>= row 5) (display "")) ; reached end of lights, do nothing
            ((>= col 5) (iter (+ row 1) 0))
            (else 
             (let ((centerLight (get-light board row col)))
               (connectRight centerLight board row col) ;; connect 4 adjacent lights
               (connectLeft centerLight board row col)
               (connectUp centerLight board row col)
               (connectDown centerLight board row col)
              (iter row (+ col 1))))))
    (iter 0 0))
    
    ;; note that these functions takes into account border cases (no wrap around)
    (define (connectRight centerLight board row col)
      (if (<= row 3)
          (centerLight 'add-neighbor! (get-light board (+ row 1) col))
          (display "")))
    
    (define (connectLeft centerLight board row col)
      (if (>= row 1)
          (centerLight 'add-neighbor! (get-light board (- row 1) col))
          (display "")))
    
    (define (connectUp centerLight board row col)
      (if (>= col 1)
          (centerLight 'add-neighbor! (get-light board row (- col 1)))
          (display "")))
    
    (define (connectDown centerLight board row col)
      (if (<= col 3)
          (centerLight 'add-neighbor! (get-light board row (+ col 1)))
          (display "")))
    
    
  ;; Print the board representation to the terminal.
  (define (print board)
    (define (iter row col)
      (cond ((>= row 5) (newline))
            ((>= col 5) (newline) (iter (+ row 1) 0))
            (else 
              (if (eq? ((get-light board row col) 'get-state) 'on)
                  (display "0")
                  (display "."))
              (iter row (+ col 1)))))
    (iter 0 0))

  ;; Define the message-passing lambda yourself.
  
    (lambda (op . args)
     (cond ((eq? op 'init!) ;; initialize the board and make connections.
            (begin
              (set! board (initialize! board (car args)))
              (connect! board)))
           ((eq? op 'print) (print board)) ;; prints the current board
           ((eq? op 'play!)   ;; play a move. Takes 2 arguments (row and col)
            (if (and (= (length args) 2) (>= (car args) 0) (<= (car args) 4) ; checks 0 <= row <= 4
                     (>= (car (cdr args)) 0) (<= (car (cdr args)) 4)) ; checks 0 <= col <= 4
                (begin               
                                      ; row     ; col
                  ((get-light board (car args) (car (cdr args))) 'toggle!)
                  ((get-light board (car args) (car (cdr args))) 'update!))
              (error "Must play a valid row/col combo: " args)))
           
           ;; extra messages used for tests...
           ((eq? op 'set-board!) (set! board (list)))
           ((eq? op 'get-board!) board)
           ((eq? op 'board-length!) (length board))
           ((eq? op 'get-light!) (get-light board 0 0))
           (else
            (error "unknown op: " op))))))

;; define, initialize, and print a test game called "game"
(define game (make-game))
(game 'init! '((on  on  on  off off)
               (off off off on  off)
               (on  on  off on  on)
               (off off off on  off)
               (on  on  on  off off))) 
(game 'print)

(generate-report)