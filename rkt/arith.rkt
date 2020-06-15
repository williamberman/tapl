#lang racket/base

(require racket/contract
         [for-syntax syntax/parse racket/base])

(provide (rename-out [arith-if if])
         
         true
         false
         succ
         pred
         is-zero?
         
         (except-out (all-from-out racket/base) if))

(define true #t)

(define false #f)

(define/contract (is-zero? num)
  (-> natural-number/c boolean?)
  (eq? 0 num))

(define/contract (succ num)
  (-> natural-number/c natural-number/c)
  (+ 1 num))

(define/contract (pred num)
  (-> natural-number/c natural-number/c)
  (if (eq? 0 num)
      0
      (- num 1)))

(define-syntax (arith-if stx)
  (syntax-parse stx
    [(_ predicate:expr consequent:expr alternative:expr)
     (syntax
      (let ([predicate-result predicate])
        (if (boolean? predicate-result)
            (if predicate-result
                consequent
                alternative)
            (error 'if "boolean expected"))))]))
