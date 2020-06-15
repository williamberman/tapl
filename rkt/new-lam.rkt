#lang racket

(require (for-syntax syntax/parse))

(provide (rename-out [new-lambda lambda]
                     [x-if if])
         (except-out (all-from-out racket) lambda if))

(define-syntax (new-lambda stx)
  (syntax-parse stx
    [(_ (x:id (~literal ::) predicate:id) body:expr)
     (syntax
      (lambda (x)
        (unless (predicate x)
          (define name (object-name predicate))
          (error 'lambda "~a expected, given ~e" name x))
        body))]))

(define-syntax (x-if stx)
  (syntax-parse stx
    [(_ predicate:expr consequent:expr alternative:expr)
     (syntax
      (let ([predicate-result predicate])
        (if (boolean? predicate-result)
            (if predicate-result
                consequent
                alternative)
            (error 'if "boolean expected"))))]))
