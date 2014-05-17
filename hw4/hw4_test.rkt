#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-n-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
  (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 27))

(sequence 3 11 2)
(sequence 3 8 3)
(sequence 3 2 1)
;'(3 5 7 9 11)
;'(3 6)
;'()
(stream-for-n-steps funny-number-stream 15)
(stream-for-n-steps dan-then-dog 5)
(stream-for-n-steps (stream-add-zero funny-number-stream) 7)
;'(1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15)
;'("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg")
;'((0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . -5) (0 . 6) (0 . 7))
(stream-for-n-steps (cycle-lists `(1 2 3) `("a" "b")) 8)
;'((1 . "a")
;  (2 . "b")
;  (3 . "a")
;  (1 . "b")
;  (2 . "a")
;  (3 . "b")
;  (1 . "a")
;  (2 . "b"))
(define my-assoc (cached-assoc `((1 1) (2 4) (3 9) (4 16) (5 25)) 3))
(my-assoc 1)
(my-assoc 2)
(my-assoc 3)
(my-assoc 1)
(my-assoc 4)
(my-assoc 1)
(my-assoc 2)
;"miss "'(1 1)
;"miss "'(2 4)
;"miss "'(3 9)
;'(1 1)
;"miss "'(4 16)
;"miss "'(1 1)
;"miss "'(2 4)
(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;"x""x""x""x""x"#t
;"x"#t
(stream-for-n-steps (cycle-lists-challenge `(1 2 3) `("a" "b")) 8)
; the same result with cycle-lists
(define my-assoc-lru (cached-assoc-lru `((1 1) (2 4) (3 9) (4 16) (5 25)) 3))
(my-assoc-lru 1)
(my-assoc-lru 2)
(my-assoc-lru 3)
(my-assoc-lru 1)
(my-assoc-lru 4)
(my-assoc-lru 1)
(my-assoc-lru 2)
;"miss "'(1 1)
;"miss "'(2 4)
;"miss "'(3 9)
;'(1 1)
;"miss "'(4 16)
;'(1 1)
;"miss "'(2 4)
