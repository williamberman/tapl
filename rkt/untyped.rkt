#lang racket/base

(require [for-syntax syntax/parse racket/base]
         racket/contract)

(provide (rename-out [lambda-abstraction lambda]                     
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
                     #%top-interaction
                     #%app))

;; TODO when printing with lambda-terms, we should perform the actual substitution of the bindings

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
(define *lambda-calculus-binding-names* (make-weak-hash))
(define *lambda-calculus-readable-converter* (make-hash))

(define-syntax (lambda-abstraction stx)
  (syntax-parse stx
    [(_ variable:id body:expr)
     (syntax
      (let ([result (lambda (variable)
                      body)])
        (hash-set! *lambda-calculus-interpretation*
                   result
                   '(lambda variable body))

        (hash-set *lambda-calculus-binding-names*
                  result
                  'variable)
        
        result))]
    [(_ (variable:id ...) body:expr)
     (syntax
      (lambda (variable ...)
        body))]))

(define *lambda-calculus-types* (make-weak-hash))

(define : (lambda (value . types)
            (hash-set! *lambda-calculus-types* value types)))

(define (type-of value)
  (hash-ref *lambda-calculus-types* value 'any))

(define/contract (interpret-as! type converter)
  (-> any/c procedure? void?)
  (hash-set! *lambda-calculus-readable-converter* (list type) converter))

(define *lambda-calculus-bindings* (make-weak-hash))

(define (try-apply-type result prev-proc)
  (when (and (hash-has-key? *lambda-calculus-types* prev-proc)
             (list? (hash-ref *lambda-calculus-types* prev-proc)))
    
    (define type (safe-tail (hash-ref *lambda-calculus-types* prev-proc)))
    
    (when (or (not (hash-has-key? *lambda-calculus-types* result))
              (equal? (hash-ref *lambda-calculus-types* result)
                      type))
      (apply : (cons result type)))))

(define (try-apply-binding result prev-proc arg)
  (define bindings
    (hash-ref *lambda-calculus-bindings* prev-proc (make-immutable-hash)))
  (define next-bindings
    (hash-set bindings (hash-ref *lambda-calculus-binding-names* result) arg))
  (hash-set *lambda-calculus-bindings* result next-bindings))

;; create the lambda-calculus-interpretation of the result
(define-syntax (#%lambda-calculus-app stx)
  (syntax-parse stx
    [(_ proc:expr arg:expr ...)
     (syntax
      (let ([result (proc arg ...)])
        (try-apply-type result proc)
        (try-apply-binding result proc arg ...)
        result))]))

(define (safe-tail lst)
  (if (null? lst)
      lst
      (cdr lst)))
