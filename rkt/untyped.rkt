#lang racket/base

(require [for-syntax syntax/parse racket/base])

(provide (rename-out [lambda-abstraction lambda])
         (except-out (all-from-out racket/base) lambda))

(define-syntax (lambda-abstraction stx)
  (syntax-parse stx
    [(_ variable:id body:expr)
     (syntax
      (lambda (variable)
        body))]))

