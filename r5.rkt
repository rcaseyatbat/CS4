
#lang racket
(require htdp/testing)

;; Ryan Casey
;; csman username: rcasey

;; Assignment 7

;; Part A: Miniproject: Lights Out Game

(define (make-light state)
  (let ((neighbors (list)))
  (lambda (op . args)
     (cond ((eq? op 'add-neighbor!)
            (if (eq? ((car args) 'get-type) 'light)
                (begin 
                  (set! neighbors
                        (cons (car args) neighbors))
                  (display (length neighbors)))
                (error "unknown state: " (car args))))
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



(define (make-game)

  ;;
  ;; Board representation:
  ;;
  (let ((board (make-board)))
  (lambda (op . args)
     (cond ((eq? op 'init!)
            ;(display (car args))
            (initialize! board (car args)))
            ;(if (eq? ((car args) 'get-type) 'light)
             ;   (begin 
              ;    (set! neighbors
               ;         (cons (car args) neighbors))
                ;  (display (length neighbors)))
                ;(error "unknown state: " (car args))))
               ;(+ value (car args))))
           ((eq? op 'print) (print board))
           ((eq? op 'set-board!) (set! board (list)))
           ;((eq? op 'get-state) state)
           ;((eq? op 'set-state!) 
           ; (if (or (eq? (car args) 'on) (eq? (car args) 'off))
            ;    (set! state (car args))
            ;    (error "unknown state: " (car args))))
           ;((eq? op 'toggle!)
           ; (if (eq? state 'on)
           ;     (set! state 'off) 
           ;     (set! state 'on)))
           ;((eq? op 'update!)
           ; (let ((f (lambda (x) (x 'toggle!))))
           ;   (for-each f neighbors))) ;; note: we will add the light we are 
                                      ;; applying 'toggle to the neighbor list.
           (else
            (error "unknown op: " op))))))

  (define (make-board)
      (let ((board (list))
            (row (list (list) (list) (list) (list) (list))))
      (set! board row);(list row row row row row))
      board))
          
  
  (define (get-light board row col)
    
    (if (and (<= 0 row) (>= 4 row))
        (if (and (<= 0 col) (>= 4 col))
            (nth-in-list (nth-in-list board row) col)
            (error "invalid column: " col))
            (error "invalid row: " row)))
  
;; returns the nth element of the input list (starting index at 0)    
   (define nth-in-list 
     (lambda (list n)
      (if (eq? n 0)
          ; this is the trivial case
          (car list)
          ; otherwise find the result in the tail of ls
          (nth-in-list (cdr list) (- n 1)))))
  
;; returns a list with the nth item in a list replaced by a new value
;; starting index is 0.   
  (define (list-with lst idx val)
  (if (null? lst)
    lst
    (cons
      (if (zero? idx)
        val
        (car lst))
      (list-with (cdr lst) (- idx 1) val))))
   
   
  (define (set-light! board row col state)
    ((get-light board row col) 'set-state! state))
  
  ;;
  ;; Helper functions:
  ;;
 
  ;; Initialize the board given a list of lists of on/off values 
  ;; by setting the corresponding lights to those values.
  (define (initialize! board init)
    ;(let ((board (make-board)))
     ; 0))
    (begin 
      (for-each g init)
      (display 11)
      ;(display (get-light board 0 0))
      (display init)
      (print board)))
   ; (display 11)
    ; (set! board init))
  
  (define (f items)
    (for-each g items))
  ;(define (lights row)
    ;(for-each convert row))
    
  
  (define (g row)
    (let ((l1 (make-light '(car row)))
            (l2 (make-light '(car (cdr row))))
            (l3 (make-light '(car (cdr (cdr row)))))
            (l4 (make-light '(car (cdr (cdr (cdr row))))))
            (l5 (make-light '(car (cdr (cdr (cdr (cdr row))))))))
      (display row)
      ;(set! (row (list l1 l2 l3 l4 l5)))
      (display row)))
            
  
  (define (init-iter! board init)
    (define (iter row col)
      (cond ((>= row 5) 'quit)
            ((>= col 5) (iter (+ row 1) 0))
            (else 
             ;(let* ((state (nth-in-list (nth-in-list board (+ row 1)) (+ col 1)))
              ;     (light (make-light state)))
               ;(set! board (list-with (nth-in-list board (+ row 1)) col light)))
               ;(display 'state)
              ;(if (edge? row col);(eq? ((get-light board row col) 'get-state) 'on)
               ;   (display "0")
                ;  (display "."))
              (iter row (+ col 1)))))
    (iter 0 0))
  
  ;; Connect all orthogonally adjacent lights to each other.
  (define (connect! board)
    (define (iter row col)
      (cond ((>= row 5) 'quit)
            ((>= col 5) (iter (+ row 1) 0))
            (else 
              (if (edge? row col);(eq? ((get-light board row col) 'get-state) 'on)
                  (display "0")
                  (display "."))
              (iter row (+ col 1)))))
    (iter 0 0))
  
  (define (edge? row col)
    (if (or (= 0 row) (= 4 row) (= 0 col) (= 4 col))
        #t
        #f))
  (define (connect-norm board row col)
    (l1 'toggle))
    ;((get-light board row col) add-neighbor! (get-light board (- row 1) col))
    ;((get-light board row col) add-neighbor! (get-light board (+ row 1) col))
    ;((get-light board row col) add-neighbor! (get-light board row (- col 1)))
    ;((get-light board row col) add-neighbor! (get-light board row (+ col 1)))
    ;((get-light board row col) add-neighbor! (get-light board row col))) ;; add own position as neighbor
  
  
  (define (left? row)
    (= row 0))
  (define (right? row)
    (= row 4))
  (define (top? col)
    (= col 0))
  (define (bottom? col)
    (= col 4))
  
                   
    
    
  ;; Print the board representation to the terminal.
  (define (print board)
    (define (iter row col)
      (cond ((>= row 5) (newline))
            ((>= col 5) (newline) (iter (+ row 1) 0))
            (else 
              (if (eq? (get-light board row col) "on");((get-light board row col) 'get-state) 'on)
                  (display "0")
                  (display "."))
              (iter row (+ col 1)))))
    (iter 0 0))

  
  




(define l1
  (make-light 'on))
(define l2
  (make-light 'off))
(define l3
  (make-light 'off))
(l1 'get-state)
(l2 'get-state)
;(l1 'add-neighbor! l2)
;(l1 'add-neighbor! l3)
(define (adds)
  (l1 'add-neighbor! l2)
  (l1 'add-neighbor! l3))

(define x (list 1 2))
(define y (list 3 4))
(define w (append x y))
(define on2 'on)
(define l4 (make-light on2))

(define samplelights (list 'on 'off 'off' 'on 'off))


(define sample (list (list 00 01 02) (list 10 11 12)))
(list-with (nth-in-list sample 1) 1 99)
(set! sample (list-with (nth-in-list sample 1) 1 99))

    
    

(define game (make-game))
(game 'init! '((on  on  on  off off)
               (off off off on  off)
               (on  on  off on  on)
               (off off off on  off)
               (on  on  on  off off)))





(generate-report)