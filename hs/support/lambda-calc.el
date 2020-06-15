;;; lamda-calc.el --- A major mode for editing Lambda Calculus files  -*- lexical-binding: t; -*-

(defgroup lambda-calc '()
  "Lambda Calculus"
  :group 'languages
  :tag "Lambda Calculus")

(defconst lambda-calc--keywords nil)
(setq lambda-calc--keywords '("lambda"))

(defconst lambda-calc--keyword-regexp nil)
(setq lambda-calc--keyword-regexp (regexp-opt lambda-calc--keywords 'words))

(defconst lambda-calc--variable-name-regexp nil)
(setq lambda-calc--variable-name-regexp  "^\\w+")

(defconst lambda-calc--abstraction-complete-regexp nil)
(setq lambda-calc--abstraction-complete-regexp "\\.")

;; TODO these faces should be part of this module and added to the group
(defconst lambda-calc--font-lock-keywords nil)
(setq lambda-calc--font-lock-keywords
      `((,lambda-calc--abstraction-complete-regexp 0 font-lock-warning-face)
	(,lambda-calc--variable-name-regexp 0 font-lock-variable-name-face)
	(,lambda-calc--keyword-regexp 0 font-lock-keyword-face)))

(defconst lambda-calc--font-lock-defaults nil)
(setq lambda-calc--font-lock-defaults
      `(,lambda-calc--font-lock-keywords))

(defvar lambda-calc-syntax-table
  nil
  "Syntax table for `lambda-calc-mode'")
(setq lambda-calc-syntax-table
      (let ((table (make-syntax-table)))
	(modify-syntax-entry ?- ". 12" table)
        (modify-syntax-entry ?\n "> " table)
      	table))

(define-derived-mode lambda-calc-mode prog-mode "Lambda Calculus"
  "Major mode for editing Lambda Calculus."
  :group 'lambda-calc
  (setq-local font-lock-defaults lambda-calc--font-lock-defaults)
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (set-syntax-table lambda-calc-syntax-table))

(add-to-list 'auto-mode-alist '("\\.untyped\\'" . lambda-calc-mode))

(provide 'lambda-calc)
