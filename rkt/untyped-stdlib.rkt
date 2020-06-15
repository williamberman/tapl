#lang s-exp "untyped.rkt"

(provide (all-defined-out))

;; Church booleans
(define true (lambda t (lambda f t)))

(define false (lambda t (lambda f f)))

(define if (lambda predicate (lambda consequent (lambda alternative
                                                  ((predicate consequent) alternative)))))

(define and (lambda a (lambda b
                        ((a b) false))))

(define or (lambda a (lambda b
                       ((a true) b))))

(define not (lambda a (a false true)))


;; Pairs
(define pair (lambda fst (lambda snd (lambda choose
                                       (choose fst snd)))))

(define first (lambda the-pair (the-pair true)))

(define second (lambda the-pair (the-pair false)))


;; Church Numerals
(define c0 (lambda scc (lambda zro zro)))
(define c1 (lambda scc (lambda zro (scc zro))))
(define c2 (lambda scc (lambda zro (scc (scc zro)))))
(define c3 (lambda scc (lambda zro (scc (scc (scc zro))))))

(define succ (lambda num (lambda scc (lambda zro ((num scc) zro)))))

(define pred (lambda num
               (let ([reducer (lambda cur ((pair (second cur)
                                                 (plus c1 (second cur)))))]
                     [init ((pair c0) c0)])
                 (first ((num reducer) init)))))

(define plus (lambda num1 (lambda num2 (lambda scc (lambda zro
                                               (let ([num1-applications ((num1 scc) zro)])
                                                 ((num2 scc) num1-applications)))))))

(define subtract (lambda num1 (lambda num2 ((num1 pred) num2))))

(define multiply (lambda num1 (lambda num2 ((num1 (plus num2)) c0))))

(define power (lambda base (lambda exponent
                                 ((exponent (multiply base)) c1))))

(define zero? (lambda num ((num (lambda _ false)) true)))

(define equal? (lambda num1 (lambda num2 ((and
                                           (zero? ((subtract num1) num2)))
                                          (zero? ((subtract num2) num1))))))


;; Lists

;; Lists are defined in terms of their fold function.
;; '(x y z) -> '(cons x (cons y (cons z nil)))
(define cons (lambda hd (lambda tl (lambda cns (lambda nl
                                                 ((cns hd) ((tl cns) nl)))))))
(define nil (lambda cns (lambda nl nl)))

(define nil? (lambda lst (let ([reducer (lambda _ (lambda _ false))])
                           ((lst reducer) true))))

(define head (lambda lst (let ([reducer (lambda hd (lambda _ hd))])
                           (lst reducer false))))

(define tail (lambda lst (let ([reducer (lambda hd (lambda acc
                                                     ((pair (second acc)) ((cons hd) (second acc)))))]
                               [init ((pair nil) nil)])
                           (first ((lst reducer) init)))))

;; Recursion combinators

;; The divergent omega combinator evaluates to itself (contains no normal form).
;; As a result, you should not evaluate it :)
;; (define omega ((lambda x (x x)) (lambda x (x x))))

;; Creates call by value recursive function
(define fix (lambda func (let ([fixed (lambda x (func (lambda y ((x x) y))))])
                           (fixed fixed))))

;; Fix point combinator in call by name evaluation
(define Y (lambda func ((lambda x (func (x x))) (lambda x (func (x x))))))

;; All together now
(define factorial (fix (lambda recur (lambda num
                                       (if (zero? num)
                                           c1
                                           ((multiply num) (recur (pred num))))))))
