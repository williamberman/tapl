#lang s-exp "arith.rkt"

(require racket/exn)

(succ 1)

(pred 2)

(if (is-zero? 0) "consequent" "alternative")

(if (is-zero? (succ (succ 1))) "consequent" "alternative")

(if (is-zero? (pred (pred 2))) "consequent" "alternative")

(with-handlers ([exn:fail? (lambda (e) (displayln (exn->string e)))])
  (if 0 "consequent" "alternative"))
