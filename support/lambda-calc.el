;;; lamda-calc.el --- A major mode for editing Lambda Calculus files  -*- lexical-binding: t; -*-

(defgroup lambda-calc '()
  "Lambda Calculus"
  :group 'languages
  :tag "Lambda Calculus")

(defconst lambda-calc--keywords
  '("lambda"))

(defconst lambda-calc--keyword-regexp
  (regexp-opt lambda-calc-keywords 'words)
  "Regular expression for Lambda Calculus keyword highlighting.")

(defconst lambda-calc--variable-name-regexp
  "^\\w+"
  "Regular expression for Lambda Calculus variable name highlighting.")

(defconst lambda-calc--font-lock-keywords
  `((,lambda-calc--variable-name-regexp 0 font-lock-variable-name-face)
    (,lambda-calc--keyword-regexp 0 font-lock-keyword-face)))

(defconst lambda-calc--font-lock-defaults
  `(,lambda-calc--font-lock-keywords)
  "Highlighting instructions for Lambda Calculus.")

(define-derived-mode lambda-calc-mode prog-mode "Lambda Calculus"
  "Major mode for editing Lambda Calculus."
  :group 'lambda-calc
  (setq-local font-lock-defaults lambda-calc-font-lock-defaults)
  (setq-local comment-start "-- ")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.untyped\\'" . lambda-calc-mode))

(provide 'lambda-calc)
