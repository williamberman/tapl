#lang racket/base

(provide set-print-mode!
         print-mode?
         (rename-out [make-lambda-abstraction lambda])

         (except-out (all-from-out racket/base)
                     lambda))

(require "core.rkt"
         "printer.rkt")
