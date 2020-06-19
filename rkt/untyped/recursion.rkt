#lang s-exp "lang.rkt"

(provide fix
         Y
         down
         factorial)

(require "boolean.rkt"
         "natural-numbers.rkt")

;; TODO, these combinators are for some reason triggering infinite loops

;; The divergent omega combinator evaluates to itself (contains no normal form).
;; As a result, you should not evaluate it :)
;; (define omega ((lambda x (x x)) (lambda x (x x))))

;; Creates call by value recursive function
(define fix (lambda func
              (let ([fixed (lambda x (func (lambda y ((x x) y))))])
                (fixed fixed))))

;; Fix point combinator in call by name evaluation
(define Y (lambda func ((lambda x (func (x x))) (lambda x (func (x x))))))

;; All together now
(define down (fix
              (lambda recur
                (lambda num
                  (((if (zero? num)) num) (recur (pred num)))))))

(define factorial (fix
                   (lambda recur
                     (lambda num
                       ((((if (zero? num))
                          c1)
                         ((multiply num) (recur (pred num)))))))))
