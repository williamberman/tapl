#lang racket/base

(provide register-printer!
         printer
         set-print-mode!
         print-mode?
         print-with)

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

(define (print-with printer-key lambda-abstraction)
  ((hash-ref *printers* printer-key) lambda-abstraction))

(register-printer! 'debug (lambda (lambda-abstraction) lambda-abstraction))
(register-printer! 'lambda-terms print-lambda-abstraction)
