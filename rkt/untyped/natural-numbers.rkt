#lang s-exp "lang.rkt"

(provide c0
         c1
         c2
         c3
         succ
         pred
         and
         subtract
         multiply
         power
         zero?
         equal?)

(require "pair.rkt"
         "boolean.rkt")

(define c0 (lambda scc
             (lambda zro zro)))

(define succ (lambda num
               (lambda scc
                 (lambda zro
                   (scc ((num scc) zro))))))

(define c1 (succ c0))
(define c2 (succ c1))
(define c3 (succ c2))

(define pred (lambda num
               (let ([reducer (lambda cur
                                ((pair (second cur))
                                 ((add c1) (second cur))))]
                     [init ((pair c0) c0)])
                 (first ((num reducer) init)))))

(define add (lambda num1
              (lambda num2
                (lambda scc
                  (lambda zro
                    (let ([num1-applications ((num1 scc) zro)])
                      ((num2 scc) num1-applications)))))))

(define subtract (lambda num1
                   (lambda num2
                     ((num2 pred) num1))))

(define multiply (lambda num1
                   (lambda num2
                     ((num1 (add num2)) c0))))

(define power (lambda base
                (lambda exponent
                  ((exponent (multiply base)) c1))))

(define zero? (lambda num
                ((num (lambda _ false)) true)))

(define equal? (lambda num1
                 (lambda num2
                   ((and
                     (zero? ((subtract num1) num2)))
                    (zero? ((subtract num2) num1))))))

