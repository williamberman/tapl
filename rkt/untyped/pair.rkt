#lang s-exp "lang.rkt"

(provide pair first second)

(require "boolean.rkt")

(define pair (lambda fst
               (lambda snd
                 (lambda choose
                   ((choose fst) snd)))))

(define first (lambda the-pair
                (the-pair true)))

(define second (lambda the-pair
                 (the-pair false)))
