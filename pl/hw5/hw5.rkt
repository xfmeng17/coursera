;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

;; Problem 2
(define (mupllist->racketlist lst)
  (if (aunit? lst)
      null
      (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) (closure env e)]
        [(closure? e) e]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)]) 
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)]) 
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (int (if (aunit? (eval-under-env (isaunit-e e) env)) 1 0))]
        [(mlet? e)
         (let* ([v (mlet-var e)]
               [exp (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons v exp) env)))]
;        [(call? e)
;         (let ([cls (eval-under-env (call-funexp e) env)])
;           (if (closure? cls)
;               (let* ([fun_name (fun-nameopt (closure-fun cls))]
;                      [cls_env (closure-env cls)]
;                      [cls_env (if fun_name (append cls_env (list (cons fun_name cls))) cls_env)]
;                      [actual (eval-under-env (call-actual e) env)]
;                      [formal (fun-formal (closure-fun cls))]
;                      [cls_env (append cls_env (list (cons formal actual)))]
;                      [fun_body (fun-body (closure-fun cls))])
;                 (eval-under-env fun_body cls_env))
;               (error "MUPL call applied to non-closure")))]
        [(call? e)
         (let ([cl (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cl)
               (let* ([fn (closure-fun cl)]
                      [bodyenv (cons (cons (fun-formal fn) arg) (closure-env cl))]
                      [bodyenv (if (fun-nameopt fn)
                                   (cons (cons (fun-nameopt fn) cl) bodyenv)
                                   bodyenv)])
                 (eval-under-env (fun-body fn) bodyenv))
               (error "MUPL function call with nonfunction")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;(define (mlet* lst_pair final_exp)
;  (letrec ([f (lambda (lst env)
;                (if (null? lst)
;                    null
;                    (let* ([str (car (car lst))]
;                           [val (eval-under-env (cdr (car lst)) env)]
;                           [bnd (cons str val)])
;                      (cons bnd (f (cdr lst) (cons bnd env))))))])
;    (eval-under-env final_exp (f lst_pair null))))
(define (mlet* bs e2)
  (cond [(null? bs) e2]
        [#t (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))]))

;(define (ifeq e1 e2 e3 e4)
;  (mlet*
;   (list (cons "_x" e1) (cons "_y" e2))
;   (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))
;; Problem 4
;(define mupl-map
;  (fun #f "map_func"
;       (fun "func" "lst"
;            (ifaunit (var "lst")
;                     (aunit)
;                     (apair (call (var "map_func") (fst (var "lst")))
;                            (call (var "func") (snd (var "lst"))))))))
(define mupl-map
  (fun "map" "f"
       (fun #f "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f") (fst (var "xs")))
                            (call (call (var "map") (var "f"))
                                  (snd (var "xs"))))))))

;(define mupl-mapAddN 
;  (mlet "map" mupl-map
;        (fun #f "i"
;             (call (var "map")
;                   (fun #f "x" (add (var "i") (var "x")))))))
(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "x"
             (call (var "map") (fun #f "y" (add (var "x") (var "y")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
