#lang racket/base

(require [for-syntax syntax/parse racket/base]
         racket/contract)

(provide (rename-out [lambda-abstraction lambda]
                     ;; [lambda-calculus-define define]
                     [#%lambda-calculus-top-interaction #%top-interaction]
                     [#%lambda-calculus-app #%app]
                     [lambda proc])
         set-print-mode!
         print-mode?
         
         :
         interpret-as!
         type-of
         (except-out (all-from-out racket/base)
                     lambda
                     ;; define
                     #%top-interaction
                     #%app))

;; Print modes
;; 1) 'debug - Print the underlying racket data type
;; 2) 'lambda-terms - If it exists, prints the lambda calculus interpretation of the datatype. Else, prints the underlying racket data type
;; 3) 'pretty (default) - If it exists, print the readable interpretation of the datatype.
;; Else, print the lambda calculus interpretation of the data type

;; Values may be typed via dynamically applied tags.

;; Readable interpretations are created by dispatching on a type tag to a function
;; which converts a lambda calculus value into a readable interpretation.

(define *print-mode* 'pretty)

(define-syntax-rule (#%lambda-calculus-top-interaction . form)
  (case *print-mode*
    ['debug form]
    ['lambda-terms (if (hash-has-key? *lambda-calculus-interpretation* form)
                       (hash-ref *lambda-calculus-interpretation* form)
                       form)]
    ['pretty (if (and (hash-has-key? *lambda-calculus-types* form)
                      (hash-has-key? *lambda-calculus-readable-converter*
                                     (hash-ref *lambda-calculus-types* form)))
                 ((hash-ref *lambda-calculus-readable-converter*
                                 (hash-ref *lambda-calculus-types* form))
                  form)
                 (if (hash-has-key? *lambda-calculus-interpretation* form)
                     (hash-ref *lambda-calculus-interpretation* form)
                     form))]
    [else (raise '#%top-interaction "No print mode ~a" *print-mode*)]))

(define/contract (set-print-mode! print-mode)
  (-> (or/c 'debug 'lambda-terms 'pretty) void?)
  (set! *print-mode* print-mode))

(define (print-mode?) *print-mode*)

(define *lambda-calculus-interpretation* (make-weak-hash))
(define *lambda-calculus-readable-converter* (make-weak-hash))

;; (define-syntax (lambda-calculus-define stx)
;;   (syntax-parse stx
;;     [(_ variable:id body:expr)
;;      (syntax
;;       ;; begin0 gives - define: not allowed in an expression context
;;       (begin
;;         (define variable body)
;;         (hash-set! *lambda-calculus-interpretation* variable 'body)
;;         variable))]))

(define-syntax (lambda-abstraction stx)
  (syntax-parse stx
    [(_ variable:id body:expr)
     (syntax
      (let ([result (lambda (variable)
                      body)])
        (hash-set! *lambda-calculus-interpretation*
                   result
                   '(lambda (variable) body))
        result))]))

(define *lambda-calculus-types* (make-weak-hash))

(define : (lambda (value . types)
            (hash-set! *lambda-calculus-types* value types)))

(define (type-of value)
  (hash-ref *lambda-calculus-types* value 'any))

(define/contract (interpret-as! type converter)
  (-> any/c procedure? void?)
  (hash-set! *lambda-calculus-readable-converter* (list type) converter))

;; create the lambda-calculus-interpretation of the result
(define-syntax (#%lambda-calculus-app stx)
  (syntax-parse stx
    [(_ proc:expr arg:expr ...)
     (syntax
      (let ([result (proc arg ...)])
        (when (and (hash-has-key? *lambda-calculus-types* proc)
                   (list? (hash-ref *lambda-calculus-types* proc)))
          
          (define type (safe-tail (hash-ref *lambda-calculus-types* proc)))
          
          (when (or (not (hash-has-key? *lambda-calculus-types* result))
                  (equal? (hash-ref *lambda-calculus-types* result)
                          type))
              (apply : (cons result type))))
        result))]))

(define (safe-tail lst)
  (if (null? lst)
      lst
      (cdr lst)))
