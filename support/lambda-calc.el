(defgroup lambda-calc '()
  "Lambda Calculus"
  :group 'languages
  :tag "Lambda Calculus")

(defface lambda-calc-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "How to highlight Lambda Calculus keywords."
  :group 'lambda-calc)

(defface lambda-calc-operator-face
  '((t (:inherit font-lock-keyword-face)))
  "How to highlight Lambda Calculus operators."
  :group 'lambda-calc)

(defface lambda-calc-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "How to highlight Lambda Calculus variable names."
  :group 'lambda-calc)

(defconst lambda-calc-keywords
  '("lambda"))

(defvar lambda-calc--keyword-regexp
  (regexp-opt lambda-calc-keywords 'words)
  "Regular expression for Lambda Calculus keyword highlighting.")

(defconst lambda-calc-operators
  '("="))

(defvar lambda-calc--operator-regexp
  (regexp-opt lambda-calc-operators 'words)
  "Regular expression for Lambda Calculus operator highlighting.")

;; TODO this is not working
(defvar lambda-calc--variable-name-regexp
  "^\\w+"
  "Regular expression for Lambda Calculus variable name highlighting.")

(defconst lambda-calc--font-lock-keywords
  `((,lambda-calc--keyword-regexp lambda-calc-keyword-face)
    (,lambda-calc--operator-regexp lambda-calc-operator-face)
    (,lambda-calc--variable-name-regexp lambda-calc-variable-name-face)))

(defvar lambda-calc-font-lock-defaults
  `(,lambda-calc--font-lock-keywords))

(define-derived-mode lambda-calc-mode prog-mode "Lambda Calculus"
  "Major mode for editing Lambda Calculus."
  :group 'lambda-calc
  (setq-local font-lock-defaults lambda-calc--font-lock-keywords)
  (setq-local comment-start "-- ")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.untyped\\'" . lambda-calc-mode))
