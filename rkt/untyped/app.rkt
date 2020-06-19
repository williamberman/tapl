#lang racket/base

(provide app
         add-app-hook!)

(require data/gvector
         "core.rkt"
         [for-syntax syntax/parse racket/base])

(define *app-hooks* (make-gvector))

(define (add-app-hook! app-hook)
  (gvector-add! *app-hooks* app-hook))

(define-syntax (app stx)
  (syntax-parse stx
    [(_ proc:expr arg:expr)
     (syntax
      (if (lambda-abstraction? proc)
          (let ([next-term (proc arg)])
            (for ([app-hook *app-hooks*])
              (app-hook #:prev-term proc #:next-term next-term #:bound arg))
            next-term)
          (proc arg)))]
    [(_ proc:expr arg:expr ...)
     (syntax (proc arg ...))]))
