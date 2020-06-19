#lang s-exp "lang.rkt"

(provide true
         false
         if
         and
         or
         not)

;; Church booleans

(define true (lambda tru
               (lambda fls
                 tru)))

(define false (lambda tru
                (lambda fls
                  fls)))

(define if (lambda predicate
             (lambda consequent
               (lambda alternative
                 ((predicate consequent) alternative)))))

(define and (lambda a
              (lambda b
                ((a b) false))))

(define or (lambda a
             (lambda b
               ((a true) b))))

(define not (lambda a
              ((a false)
               true)))
