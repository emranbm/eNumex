;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones

(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct var  (name) #:transparent)  ;; a variable
(struct apair (first second) #:transparent)

(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct mult  (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg (e))
(struct islthan (e1 e2) #:transparent)

(struct ifzero (e1 e2 e3) #:transparent)
(struct ifgthan (e1 e2 e3 e4) #:transparent)
(struct mlet (s e1 e2) #:transparent)

(struct first (apair) #:transparent)
(struct second (apair) #:transparent)

(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (exp)     #:transparent) ;; if exp is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; General-purpose functions:
(define (listfirst lst)
  (list-ref lst 0))

(define (racket-to-numex r)
  (cond [(var? r) r]
        [(int? r) r]
        [(number? r) (int r)]
        [#t (error "Invalid type")]
         )
  )

(define (numex-to-racket r)
  (cond [(int? r) (int-num r)]
        [(number? r) r]
        [(string? r) r]
        [#t (error "Invalid type")]
         )
  )

(define (racketList-to-numexList rlist)
  (cond [(equal? (length rlist) 0) (munit)]
        [#t (apair (listfirst rlist) (racketList-to-numexList (rest rlist)))]
         ))

;; Problem 1

(define (racketlist->numexlist xs)
  ;(let ([lst (map racket-to-numex xs)])
    (racketList-to-numexList xs)
;    )
  )

(define (numexlist->racketlist xs)
  (cond [(munit? xs) '()]
        [#t (append (list (apair-first xs)) (numexlist->racketlist (apair-second xs)))]
   )
  )

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str e)
  (cond [(null? env) (error "unbound variable during evaluation" str e env)]
        [(equal? (car (listfirst env)) str) (cdr (listfirst env))]
        [#t (envlookup (rest env) str e)]
        )
  )

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e) e]
        [(munit? e) e]
        [(closure? e) e]
        [(var? e) 
         (eval-under-env (envlookup env (var-name e) e) env)]
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
                 [#t (error "NUMEX negation is just for int type")]
           ))]
        [(fun? e)
         (closure env e)]
        [(islthan? e)
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (< (int-num v1) (int-num v2))
                   (int 1)
                   (int 0))
               (error "Illegal type")
           ))]
        [(ifzero? e)
         (let ([v1 (eval-under-env (ifzero-e1 e) env)])
           (if (int? v1)
               (if (equal? v1 (int 0))
                   (eval-under-env (ifzero-e2 e) env)
                   (eval-under-env (ifzero-e3 e) env))
               (error "First arg of ifzero is not of type int")
               ))]
        [(ifgthan? e)
         (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
               [v2 (eval-under-env (ifgthan-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgthan-e3 e) env)
                   (eval-under-env (ifgthan-e4 e) env))
               (error "Illegal type")
               ))]
        [(mlet? e)
         (let ([newEnv (append (list (cons (mlet-s e) (eval-under-env (mlet-e1 e) env))) env)])
           (eval-under-env (mlet-e2 e) newEnv)
           )]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let ([body (fun-body (closure-fun v1))]
                     [newEnv (append (list (cons (fun-formal (closure-fun v1)) v2)) (closure-env v1))])
                 (if (null? (fun-nameopt (closure-fun v1)))
                     (eval-under-env body newEnv)
                     (eval-under-env body (append (list (cons (fun-nameopt (closure-fun v1)) v1)) newEnv))
                     )
                 )
               (error (format "~v is not a function" v1))))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-first e) env)]
               [v2 (eval-under-env (apair-second e) env)])
           (apair v1 v2))]
        [(first? e)
         (let ([p (eval-under-env (first-apair e) env)])
           (if (apair? p)
             (eval-under-env (apair-first p) env)
             (error (format "~v is not an apair" p))))]
        [(second? e)
         (let ([p (eval-under-env (second-apair e) env)])
           (if (apair? p)
             (eval-under-env (apair-second p) env)
             (error (format "~v is not an apair" p))))]
        [(ismunit? e)
         (let ([v1 (eval-under-env (ismunit-exp e) env)])
           (if (munit? v1)
               (int 1)
               (int 0)))]
        [#t (error (format "Bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifmunit e1 e2 e3)
  (ifzero (add (ismunit e1) (int -1)) e2 e3)
   )

(define (mlet* bs e2)
  (if (null? bs)
      e2
      (let ([binding (listfirst bs)])
        (mlet (car binding) (cdr binding) (mlet* (rest bs) e2))
        )))

(define (ifeq e1 e2 e3 e4)
  (ifzero (add e1 (neg e2)) e3 e4)
   )

;; Problem 4

(define numex-map
  (fun "f1" "f" (fun "f2" "list" (ifmunit (var "list")
                                          (munit)
                                          (apair (call (var "f") (first (var "list"))) (call (var "f2") (second (var "list")))))
                                  )))

(define numex-mapAddN
  (mlet "map" numex-map
        "CHANGE (notice map is now in NUMEX scope)"))
