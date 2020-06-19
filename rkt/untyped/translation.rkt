#lang racket/base

(provide define-translation
         tag-translation!
         print-readable-translation
         
         (rename-out [x-set-translation-lc->readable! set-translation-lc->readable!]
                     [x-set-translation-readable->lc! set-translation-readable->lc!]
                     [get-lc->readable-translator can-print-readable-translation?]))

(define *translations* (make-hash))
(define *translation-tags* (make-weak-hash))

(define-struct translation (tag [lc->readable #:mutable] [readable->lc #:mutable]))

(define (define-translation tag-name)
  (hash-set! *translations* tag-name (make-translation tag-name #f #f)))

(define (x-set-translation-lc->readable! tag-name translator)
  (set-translation-lc->readable! (hash-ref *translations* tag-name) translator))

(define (x-set-translation-readable->lc! tag-name translator)
  (set-translation-readable->lc! (hash-ref *translations* tag-name) translator))

(define (tag-translation! lambda-calculus-term tag-name)
  (hash-set! *translation-tags* lambda-calculus-term tag-name))

(define (get-lc->readable-translator lambda-calculus-term)
  (and (hash-has-key? *translation-tags* lambda-calculus-term)
       (hash-has-key? *translations*
                      (hash-ref *translation-tags* lambda-calculus-term))
       (translation-lc->readable (hash-has-key? *translations*
                                                (hash-ref *translation-tags* lambda-calculus-term)))))

(define (print-readable-translation lambda-calculus-term)
  (define translator (get-lc->readable-translator lambda-calculus-term))
  (if translator
      (translator lambda-calculus-term)
      (raise "No translator for ~s" lambda-calculus-term)))
