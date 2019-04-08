#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; for test
(define ones (lambda () (cons 1 ones)))
(define a 2)

;; put your code below
;; 1. function sequence
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2. function string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3. function list-nth-mod
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4. function stream-for-n-steps
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([stream-return (s)])
        (cons (car stream-return) (stream-for-n-steps (cdr stream-return) (- n 1))))))

;; 5. stream funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- 0 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 6. stream dan-then-dog
(define dan-then-dog
  (letrec ([f (lambda (x) (if x (cons "dan.jpg" (lambda () (f #f)))
                              (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))

;; 7. function stream-add-zero
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))
                              
;; 8. function cycle-lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; 9. function vector-assoc
(define (vector-assoc v vec)
  (letrec ([f (lambda (i) (if (>= i (vector-length vec))
                              #f
                              (let ([p (vector-ref vec i)])
                                (if (pair? p)
                                    (if (equal? (car p) v)
                                        p
                                        (f (+ i 1)))
                                    (f (+ i 1))))))])
    (f 0)))

;; 10. function cached-assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [next 0]
           [helper (lambda (v)
                     (let ([cached-ans (vector-assoc v memo)])
                       (if cached-ans
                           cached-ans
                           (let ([list-ans (assoc v xs)])
                             (begin
                               (vector-set! memo next list-ans)
                               (set! next (remainder (+ next 1) n))
                               list-ans)))))])
  helper))

;; 11. macro (while-less e1 do e2)
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([x e1])
       (letrec ([helper (lambda ()
                          (let ([y e2])
                            (if (or (not (number? y)) (>= y x))
                                #t
                                (helper))))])
         (helper)))]))
                    
        