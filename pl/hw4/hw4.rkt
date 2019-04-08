#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1. sequence
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2. string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3. list-nth-mod
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4. stream-for-n-steps

;(define (stream-for-n-steps s n)
;  (if (= n 0)
;      null
;      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; wrong version above case
;; each thunk in the stream should be evaluated at most once, which is why the code below
;; has a let-expression rather than (car (s)) and (cdr (s))
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))
  
;; 5. funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 6. dan-then-dog
;(define dan-then-dog
;  (letrec ([f (lambda (flag)
;                (if (flag)
;                    (cons "dan.jpg" (lambda () (f #f)))
;                    (cons "dog.jpg" (lambda () (f #t)))))])
;    (lambda () (f #t))))

;; I write (flag) but not flag above, parentheses matters!!!

(define dan-then-dog
  (letrec ([f (lambda (b) 
                (if b
                    (cons "dan.jpg" (lambda () (f #f)))
                    (cons "dog.jpg" (lambda () (f #t)))))])
    (lambda () (f #t))))

;; 7. stream-add-zero
(define (stream-add-zero s)
  (lambda ()
    (let ([next (s)])
      (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))

;; 8. cycle-lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; 9. vector-assoc
(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
                (if (>= x (vector-length vec))
                    #f
                    (if (pair? (vector-ref vec x))
                        (if (equal? v (car (vector-ref vec x)))
                            (vector-ref vec x)
                            (f (+ x 1)))
                        (f (+ x 1)))))])
    (f 0)))

;; 10. cached-assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [next 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (begin
                          (vector-set! memo next new-ans)
                          (set! next (remainder (+ next 1) n)) new-ans)))))])
    f))

;; 11. macro
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([x e1])
       (letrec ([loop (lambda ()
                        (let ([y e2])
                          (if (or (not (number? y)) (>= y x))
                              #t
                              (loop))))])
         (loop)))]))


