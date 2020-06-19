#lang racket/base

(provide set-translation-tags*!
         translation-tags-of
         apply-next-tag!
         any)

(require "translation.rkt")

(define *tags* (make-weak-hash))

(define any 'any)

(define set-translation-tags*!
  (lambda (lambda-calculus-term . tags)
    (tag-translation! lambda-calculus-term (car tags))
    (hash-set! *tags* lambda-calculus-term tags)))

(define (translation-tags-of lambda-calculus-term)
  (hash-ref *tags* lambda-calculus-term any))

(define (apply-next-tag! prev-term next-term)
  (define tags (cdr (hash-ref *tags* prev-term)))
  (apply set-translation-tags*! (cons next-term tags)))
