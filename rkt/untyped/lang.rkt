#lang racket/base

(provide set-print-mode!
         print-mode?
         (rename-out [make-lambda-abstraction lambda]
                     [top-interaction #%top-interaction]
                     [app #%app])

         (except-out (all-from-out racket/base)
                     lambda
                     #%top-interaction
                     #%app))

(require "core.rkt"
         "printer.rkt"
         "top-interaction.rkt"
         "app.rkt")




