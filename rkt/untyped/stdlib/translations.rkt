#lang racket/base

(require "../translation-tagger.rkt"
         "../translation.rkt"
         "../app.rkt"        
         "boolean.rkt"
         "pair.rkt"
         "natural-numbers.rkt"
         "list.rkt"
         (only-in racket/base
                  [if rkt-if]
                  [car rkt-car]
                  [cdr rkt-cdr]
                  [cadr rkt-cadr]
                  [cons rkt-cons]))

(add-app-hook! (lambda (#:prev-term prev-term #:next-term next-term #:bound bound)
                 (apply-next-tag! prev-term next-term)))

(define boolean 'boolean)
(define-translation boolean)

(set-translation-lc->readable!
 boolean
 (lambda (lc-bool)
   ((lc-bool #t) #f)))

(set-translation-readable->lc!
 boolean
 (lambda (rkt-bool)
   (if rkt-bool true false)))

(set-translation-tags*! true boolean)
(set-translation-tags*! false boolean)
(set-translation-tags*! if boolean any any)
(set-translation-tags*! and boolean boolean boolean)
(set-translation-tags*! or boolean boolean boolean)
(set-translation-tags*! not boolean boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pair-tag 'pair)
(define-translation pair-tag)

(set-translation-lc->readable!
 pair-tag
 (lambda (lc-pair)
   (list (first lc-pair) (second lc-pair))))

(set-translation-readable->lc!
 pair-tag
 (lambda (rkt-pair)
   ((pair (rkt-car rkt-pair))
    (rkt-cadr rkt-pair))))

(set-translation-tags*! pair-tag any any pair-tag)
(set-translation-tags*! first pair-tag any)
(set-translation-tags*! second pair-tag any)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define natural 'natural)
(define-translation natural)

(set-translation-lc->readable!
 natural
 (lambda (lc-num) ((lc-num add1) 0)))

(set-translation-readable->lc!
 natural
 (lambda (rkt-num)
   (define lc-num c0)
   (for ([_ (in-range rkt-num)])
     (set! lc-num (succ lc-num)))
   lc-num))

(set-translation-tags*! c0 natural)
(set-translation-tags*! c1 natural)
(set-translation-tags*! c2 natural)
(set-translation-tags*! c3 natural)
(set-translation-tags*! add natural natural natural)
(set-translation-tags*! subtract natural)
(set-translation-tags*! multiply natural)
(set-translation-tags*! power natural)
(set-translation-tags*! zero? natural)
(set-translation-tags*! equal? natural)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-tag 'list)
(define-translation list-tag)

(set-translation-lc->readable!
 list-tag
 (lambda (lc-lst)
   ((lc-lst
     (lambda (hd)
       (lambda (tl)
         (rkt-cons hd tl))))
    (list))))

(set-translation-readable->lc!
 list-tag
 (lambda (rkt-lst)
   (define (recur rkt-lst lc-lst)
     (if (null? rkt-lst)
         lc-lst
         (recur (rkt-cdr rkt-lst) (cons (rkt-car rkt-lst) lc-lst))))
   (recur (reverse rkt-lst) nil)))

(set-translation-tags*! cons any list-tag list-tag)
(set-translation-tags*! nil list-tag)
(set-translation-tags*! nil? list-tag boolean)
(set-translation-tags*! head list-tag any)
(set-translation-tags*! tail list-tag list-tag)

