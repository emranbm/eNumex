;; PL Project - Fall 2017
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Add the missing ones

(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct var  (string) #:transparent)  ;; a string
(struct apair (first second) #:transparent) 
(struct add  (e1 e2)  #:transparent)  ;; add two expressions


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
        [(string? r) (var r)]
        [#t "ERROR: Invalid type"]
         )
  )

(define (numex-to-racket r)
  (cond [(var? r) (var-string r)]
        [(int? r) (int-num r)]
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
