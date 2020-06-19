#lang racket/base

(provide top-interaction)

(require "printer.rkt"
         "translation.rkt")

(register-printer! 'pretty
                   (lambda (term)
                     (if (can-print-readable-translation? term)
                         (print-readable-translation term)
                         (print-with 'lambda-terms term))))

(set-print-mode! 'pretty)

(define-syntax-rule (top-interaction . form)
  form
  ;; (printer form)
  )
