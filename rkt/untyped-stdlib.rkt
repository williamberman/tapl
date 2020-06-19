#lang s-exp "untyped.rkt"

(require (only-in racket/base [cons rkt-cons]))

(provide (all-defined-out))

;; Church booleans
(define true (lambda tru (lambda fls tru)))
(: true 'boolean)

(define false (lambda tru (lambda fls fls)))
(: false 'boolean)

(define if (lambda predicate (lambda consequent (lambda alternative
                                                  ((predicate consequent) alternative)))))
(: if 'boolean 'any 'any)

(define and (lambda a (lambda b
                        ((a b) false))))
(: and 'boolean 'boolean 'boolean)

(define or (lambda a (lambda b
                       ((a true) b))))
(: or 'boolean 'boolean 'boolean)

(define not (lambda a ((a false) true)))
(: not 'boolean 'boolean)

(interpret-as! 'boolean (proc (boolean) ((boolean #t) #f)))

;; Pairs
(define pair (lambda fst (lambda snd (lambda choose
                                       ((choose fst) snd)))))
(: pair 'any 'any 'pair)

(define first (lambda the-pair (the-pair true)))
(: first 'pair 'any)

(define second (lambda the-pair (the-pair false)))
(: second 'pair 'any)

(interpret-as! 'pair (proc (pair) (list (first pair) (second pair))))

;; Church Numerals
(define c0 (lambda scc (lambda zro zro)))
(: c0 'natural)

(define succ (lambda num (lambda scc (lambda zro (scc ((num scc) zro))))))
(: succ 'natural 'natural)

(define c1 (succ c0))
(define c2 (succ c1))
(define c3 (succ c2))

(define pred (lambda num
               (let ([reducer (lambda cur ((pair (second cur))
                                           ((add c1) (second cur))))]
                     [init ((pair c0) c0)])
                 (first ((num reducer) init)))))
(: pred 'natural 'natural)

(define add (lambda num1 (lambda num2 (lambda scc (lambda zro
                                               (let ([num1-applications ((num1 scc) zro)])
                                                 ((num2 scc) num1-applications)))))))
(: add 'natural 'natural 'natural)

(define subtract (lambda num1 (lambda num2 ((num2 pred) num1))))
(: subtract 'natural 'natural 'natural)

(define multiply (lambda num1 (lambda num2 ((num1 (add num2)) c0))))
(: multiply 'natural 'natural 'natural)

(define power (lambda base (lambda exponent
                             ((exponent (multiply base)) c1))))
(: power 'natural 'natural 'natural)

(define zero? (lambda num ((num (lambda _ false)) true)))
(: zero? 'natural 'boolean)

(define equal? (lambda num1 (lambda num2 ((and
                                           (zero? ((subtract num1) num2)))
                                          (zero? ((subtract num2) num1))))))
(: equal? 'natural 'natural 'boolean)

(interpret-as! 'natural (proc (num) ((num add1) 0)))


;; Lists

;; Lists are defined in terms of their fold function.
;; '(x y z) -> '(reducer x (reducer y (reducer z nil)))
(define cons (lambda hd (lambda tl (lambda cns (lambda nl
                                                 ((cns hd) ((tl cns) nl)))))))
(: cons 'any 'list 'list)

(define nil (lambda cns (lambda nl nl)))
(: nil 'list)

(define nil? (lambda lst (let ([reducer (lambda _ (lambda _ false))])
                           ((lst reducer) true))))
(: nil? 'list 'boolean)

(define head (lambda lst (let ([reducer (lambda hd (lambda _ hd))])
                           (lst reducer false))))
(: head 'list 'any)

(define tail (lambda lst (let ([reducer (lambda hd (lambda acc
                                                     ((pair (second acc)) ((cons hd) (second acc)))))]
                               [init ((pair nil) nil)])
                           (first ((lst reducer) init)))))
(: tail 'list 'any)

(interpret-as! 'list (proc (lst) ((lst (proc (hd) (proc (tl) (rkt-cons hd tl))))
                                  (list))))

;; Recursion combinators

;; TODO, these combinators are for some reason triggering infinite loops

;; The divergent omega combinator evaluates to itself (contains no normal form).
;; As a result, you should not evaluate it :)
;; (define omega ((lambda x (x x)) (lambda x (x x))))

;; Creates call by value recursive function
(define fix (lambda func (let ([fixed (lambda x (func (lambda y ((x x) y))))])
                           (fixed fixed))))

;; Fix point combinator in call by name evaluation
(define Y (lambda func ((lambda x (func (x x))) (lambda x (func (x x))))))

;; All together now
(define down (fix (lambda recur (lambda num (((if (zero? num)) num) (recur (pred num)))))))

(define factorial (fix (lambda recur (lambda num
                                       ((((if (zero? num))
                                          c1)
                                         ((multiply num) (recur (pred num)))))))))
