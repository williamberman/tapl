#lang racket/base

(require [for-syntax syntax/parse racket/base])

(provide (rename-out [lambda-abstraction-stx make-lambda-abstraction])
         lambda-abstraction-binding
         lambda-abstraction-body
         print-lambda-abstraction)

(define-syntax (lambda-abstraction-stx stx)
  (syntax-parse stx
    [(_ variable:id body:expr)
     (syntax      
      (make-lambda-abstraction 'variable 'body (lambda (variable) body)))]))

(define-struct lambda-abstraction (binding body func)
  #:property prop:procedure (struct-field-index func))

(define (print-lambda-abstraction lambda-abstraction)
  `(lambda ,(lambda-abstraction-binding lambda-abstraction)
     ,(lambda-abstraction-body lambda-abstraction)))
