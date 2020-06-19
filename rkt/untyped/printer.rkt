#lang racket/base

(provide register-printer!
         printer
         set-print-mode!
         print-mode?)

(require "core.rkt")

(define *print-mode* 'lambda-terms)

(define *printers* (make-hash))

(define (register-printer! printer-key printer-function)
  (hash-set! *printers* printer-key printer-function))

(define (set-print-mode! print-mode)
  (set! *print-mode* print-mode))

(define (printer lambda-abstraction)
  ((hash-ref *printers* *print-mode*) lambda-abstraction))

(define (print-mode?) *print-mode*)

(register-printer! 'debug (lambda (lambda-abstraction) lambda-abstraction))
(register-printer! 'lambda-terms print-lambda-abstraction)
