
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (< high low) 
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
   (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (raise "list-nth-mod: negative number" #t)]
        [(null? xs) (raise "list-nth-mod: empty list" #t)]
        [#t (car (list-tail xs (remainder n (length xs))))]))


(define (stream-for-n-steps s n)
  (cond [(<= n 0) null]
        [#t (let ([tmp (s)])
              (cons (car tmp) (stream-for-n-steps (cdr tmp) (- n 1))))]))

(define funny-number-stream
  (letrec ([f (lambda (x) 
                (cons (if (= 0 (remainder x 5)) (- 0 x) x) (lambda() (f (+ 1 x)))))])    
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) 
                (cons (if x "dan.jpg" "dog.jpg") (lambda() (f (not x)))))])    
    (lambda () (f #t))))

(define (stream-add-zero s)
  (letrec ([f (lambda (stream) 
                (cons (cons 0 (car (stream))) (lambda() (f (cdr (stream))))))])    
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) 
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) 
                      (lambda() (f (+ 1 x)))))])    
    (lambda () (f 0))))


(define (vector-assoc v vec)
  (letrec ([helper (lambda (n) 
                    (cond [(>= n (vector-length vec)) #f]
                          [(not (pair? (vector-ref vec n))) (helper (+ 1 n))]
                          [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                          [#t (helper (+ 1 n))]))]) 
    (helper 0)))


(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)] 
           [current 0] )
    (lambda (v) 
      (letrec ([r (vector-assoc v cache)]) 
        (if r 
            r 
            (letrec ([xsr (assoc v xs)])
                    (if (not xsr) 
                         #f
                        (begin ;(print "miss ")
                               (vector-set! cache current xsr) 
                               (set! current (remainder (+ 1 current) n))
                                xsr))))))))

(define-syntax while-less
  (syntax-rules (do)     
    [(while-less e1 do e2)
     (letrec ([r1 e1]
              [r2 (lambda ()(cons e2 (lambda () r2)))]
              [recursive (lambda (x) 
                           (letrec ([tmp (x)])
                            (if (< (begin (cdr tmp)(car tmp)) r1) 
                                (recursive ((cdr tmp))) 
                                (car tmp) )))]) 
       (begin (recursive r2) #t))])) 


(define (cycle-lists-challenge xs ys)
  (letrec ([f (lambda (x y) 
                (cons (cons (if (null? x) (car xs) (car x))
                            (if (null? y) (car ys) (car y))) 
                      (lambda() (f (if (null? x) (cdr xs) (cdr x))
                                   (if (null? y) (cdr ys) (cdr y))                                   
                                   ))))])    
    (lambda () (f xs ys))))

; if origin key is (k,v),
; the data in cache is (priority,k,v)
(define (vector-assoc-lru v vec)
  (letrec ([helper (lambda (n) 
                    (cond [(>= n (vector-length vec)) #f]
                          [(not (pair? (vector-ref vec n))) (helper (+ 1 n))]
                          [(equal? (cadr (vector-ref vec n)) v) (vector-ref vec n)]
                          [#t (helper (+ 1 n))]))]) 
    (helper 0)))

(define (cached-assoc-lru xs n)
  (letrec ([cache    (make-vector n #f)]
           [num 0] 
           [remove (lambda(i)
                     (begin
                     (vector-map! (lambda (data) 
                                    (cond[(not data) #f]
                                         [(= i (car data)) #f]
                                         [(> i (car data)) data]
                                         [#t (cons (- (car data) 1) (cdr data))])) 
                                  cache)
                     (set! num (- num 1))))]
           [add (lambda (item)
                  (begin 
                    (vector-set! cache (vector-memv #f cache)(cons num item))
                    (set! num (+ 1 num))))])
    (lambda (v) 
      (letrec ([r (vector-assoc-lru v cache)]) 
        (if r 
            (begin (remove (car r)) (add (cdr r)) (cdr r)) 
            (letrec ([xsr (assoc v xs)])
                    (if (not xsr) 
                         #f
                        (if (= n num) 
                            (begin ;(print "miss ")
                                   (remove 0)(add xsr) xsr)
                            (begin ;(print "miss ")
                                   (add xsr) xsr)
                            ))))))))


