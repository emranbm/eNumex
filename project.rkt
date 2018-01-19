;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones

(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct bool (val) #:transparent) ;; a boolean, e.g, (bool #t)
(struct var  (name) #:transparent)  ;; a variable
(struct apair (first second) #:transparent)

(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct mult  (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg (e))


(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; General-purpose functions
(define (racket-to-numex r)
  (cond [(var? r) r]
        [(int? r) r]
        [(number? r) (int r)]
        [#t "ERROR: Invalid type"]
         )
  )

(define (numex-to-racket r)
  (cond [(int? r) (int-num r)]
        [(number? r) r]
        [(string? r) r]
        [#t "ERROR: Invalid type"]
         )
  )

(define (racketList-to-numexList rlist)
  (cond [(equal? (length rlist) 0) (munit)]
        [#t (apair (first rlist) (racketList-to-numexList (rest rlist)))]
         ))

;; Problem 1

(define (racketlist->numexlist xs)
  (let ([lst (map racket-to-numex xs)])
    (racketList-to-numexList lst)
    )
  )

(define (numexlist->racketlist xs)
  (cond [(munit? xs) '()]
        [#t (append (list (numex-to-racket (apair-first xs))) (numexlist->racketlist (apair-second xs)))]
   )
  )

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (first env)) str) (cdr (first env))]
        [#t (envlookup (rest env) str)]
        )
  )

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e) e]
        [(bool? e) e]
        [(var? e) 
         (envlookup env (var-name e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "NUMEX addition applied to non-number")))]
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (* (int-num v1) 
                       (int-num v2)))
               (error "NUMEX multiplication applied to non-number")))]
        [(neg? e)
         (let ([v (eval-under-env (neg-e e) env)])
           (cond [(int? v) (int (- (int-num v)))]
                 [(bool? v) (bool (not (bool-val v)))]
                 [#t (error "NUMEX negation is just for int and bool types")]
           ))]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))