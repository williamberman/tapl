#lang s-exp "new-lam.rkt"

(define foo (lambda (x :: integer?) (+ x 1)))

(foo 1)

;; (if 1 "first" "second")

(let ([foo 1]) foo)

;; (foo "bar")
