;;; trashtalk-mode.el --- Major mode for editing Trashtalk files -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Trashtalk Contributors
;; Keywords: languages
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A major mode for editing Trashtalk (.trash) files.
;; Trashtalk is a Smalltalk-inspired DSL that compiles to Bash.
;;
;; Features:
;; - Syntax highlighting for keywords, operators, strings, comments
;; - Automatic indentation
;; - imenu support for navigating methods
;; - Comment/uncomment support
;;
;; Installation:
;;   Add to your init.el:
;;     (add-to-list 'load-path "/path/to/trashtalk/emacs")
;;     (require 'trashtalk-mode)
;;
;;   Or with use-package:
;;     (use-package trashtalk-mode
;;       :load-path "/path/to/trashtalk/emacs"
;;       :mode "\\.trash\\'")

;;; Code:

(require 'font-lock)

;;; Customization

(defgroup trashtalk nil
  "Major mode for editing Trashtalk files."
  :group 'languages
  :prefix "trashtalk-")

(defcustom trashtalk-indent-offset 2
  "Number of spaces for each indentation level in Trashtalk mode."
  :type 'integer
  :group 'trashtalk)

;;; Syntax Table

(defvar trashtalk-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments start with # and go to end of line
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Brackets
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Operators and punctuation
    (modify-syntax-entry ?@ "'" table)  ; @ is a prefix
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?| "$" table)  ; | for locals
    (modify-syntax-entry ?$ "/" table)  ; $ starts bash escapes
    (modify-syntax-entry ?_ "_" table)  ; _ is word constituent

    table)
  "Syntax table for `trashtalk-mode'.")

;;; Font Lock (Syntax Highlighting)

(defvar trashtalk-font-lock-keywords
  (let* (;; Class definition keywords
         (class-keywords '("subclass:" "trait"))
         ;; Declaration keywords
         (decl-keywords '("instanceVars:" "classInstanceVars:" "include:"
                          "package:" "import:"))
         ;; Method definition keywords
         (method-keywords '("method:" "classMethod:" "rawMethod:" "rawClassMethod:"))
         ;; Build regexps
         (class-keywords-regexp (regexp-opt class-keywords 'words))
         (decl-keywords-regexp (regexp-opt decl-keywords))
         (method-keywords-regexp (regexp-opt method-keywords)))

    `(
      ;; Comments (handled by syntax table, but ensure they're styled)
      ("^\\s-*#.*$" . font-lock-comment-face)

      ;; Class/trait definition: "Counter subclass: Object" or "Debuggable trait"
      (,(concat "^\\s-*\\([A-Z][a-zA-Z0-9_]*\\)\\s-+" class-keywords-regexp)
       (1 font-lock-type-face)
       (2 font-lock-keyword-face))

      ;; Superclass after subclass:
      ("subclass:\\s-+\\([A-Z][a-zA-Z0-9_:]*\\|nil\\)"
       (1 font-lock-type-face))

      ;; Declaration keywords
      (,decl-keywords-regexp . font-lock-keyword-face)

      ;; Trait names after include:
      ("include:\\s-+\\([A-Z][a-zA-Z0-9_]*\\)"
       (1 font-lock-type-face))

      ;; Method definitions
      (,method-keywords-regexp . font-lock-keyword-face)

      ;; Method name after method keyword (handles keyword methods like "at: x put: y")
      (,(concat method-keywords-regexp "\\s-+\\([a-zA-Z_][a-zA-Z0-9_:]*\\)")
       (1 font-lock-function-name-face))

      ;; @ message send prefix
      ("@\\s-+\\([a-zA-Z_$][a-zA-Z0-9_]*\\)"
       (0 font-lock-preprocessor-face)
       (1 font-lock-variable-name-face t))

      ;; self and super
      ("\\<\\(self\\|super\\)\\>" . font-lock-builtin-face)

      ;; Return operator
      ("\\^" . font-lock-keyword-face)

      ;; Assignment operator
      (":=" . font-lock-keyword-face)

      ;; Local variable declarations: | var1 var2 |
      ("\\(|\\)\\([^|]*\\)\\(|\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face)
       (3 font-lock-keyword-face))

      ;; Instance variable defaults in declaration: value:0 step:1
      ("\\<\\([a-z][a-zA-Z0-9_]*\\):\\([^[:space:]]*\\)"
       (1 font-lock-variable-name-face)
       (2 font-lock-constant-face))

      ;; Class names (capitalized identifiers)
      ("\\<\\([A-Z][a-zA-Z0-9_]*\\)\\>" . font-lock-type-face)

      ;; Bash variable interpolation
      ("\\$[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-variable-name-face)
      ("\\${[^}]+}" . font-lock-variable-name-face)
      ("\\$(" . font-lock-preprocessor-face)

      ;; Numbers
      ("\\<-?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)

      ;; Block arguments: [:x :y | ...]
      ("\\[\\s-*\\(\\(:[a-z][a-zA-Z0-9_]*\\s-*\\)+\\)"
       (1 font-lock-variable-name-face))
      ))
  "Font lock keywords for `trashtalk-mode'.")

;;; Indentation

(defun trashtalk-indent-line ()
  "Indent current line as Trashtalk code."
  (interactive)
  (let ((indent (trashtalk-calculate-indentation))
        (pos (- (point-max) (point))))
    (when indent
      (beginning-of-line)
      (if (looking-at "^\\s-*")
          (replace-match ""))
      (indent-to indent)
      ;; Move point back to where it was relative to end of line
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun trashtalk-calculate-indentation ()
  "Calculate the indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (let ((current-line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
      (cond
       ;; Class or trait definition at column 0
       ((string-match "^[A-Z][a-zA-Z0-9_]*\\s-+\\(subclass:\\|trait\\)" current-line)
        0)

       ;; Closing bracket - match opening bracket's line
       ((string-match "^\\s-*\\]" current-line)
        (trashtalk-find-matching-bracket-indent))

       ;; Lines starting with method/classMethod/rawMethod keywords
       ((string-match "^\\s-*\\(method:\\|classMethod:\\|rawMethod:\\|rawClassMethod:\\)"
                      current-line)
        trashtalk-indent-offset)

       ;; Lines starting with instanceVars/classInstanceVars/include
       ((string-match "^\\s-*\\(instanceVars:\\|classInstanceVars:\\|include:\\)"
                      current-line)
        trashtalk-indent-offset)

       ;; Default: base on previous non-blank line
       (t
        (trashtalk-previous-line-indent))))))

(defun trashtalk-find-matching-bracket-indent ()
  "Find the indentation of the line with the matching opening bracket."
  (save-excursion
    (beginning-of-line)
    (when (search-forward "]" (line-end-position) t)
      (backward-char)
      (condition-case nil
          (progn
            (backward-sexp)
            (current-indentation))
        (error trashtalk-indent-offset)))))

(defun trashtalk-previous-line-indent ()
  "Calculate indent based on previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at "^\\s-*$"))
      (forward-line -1))
    (let* ((prev-indent (current-indentation))
           (prev-line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
      (cond
       ;; After opening bracket that's not closed on same line
       ((and (string-match "\\[" prev-line)
             (not (string-match "\\]" prev-line)))
        (+ prev-indent trashtalk-indent-offset))

       ;; After method definition line (ends with [)
       ((string-match "\\[\\s-*$" prev-line)
        (+ prev-indent trashtalk-indent-offset))

       ;; After class definition
       ((string-match "^[A-Z][a-zA-Z0-9_]*\\s-+\\(subclass:\\|trait\\)" prev-line)
        trashtalk-indent-offset)

       ;; Default: same as previous line
       (t prev-indent)))))

;;; Imenu Support

(defvar trashtalk-imenu-generic-expression
  `(("Methods"
     ,(concat "^\\s-*\\(method:\\|rawMethod:\\)\\s-+"
              "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)")
     2)
    ("Class Methods"
     ,(concat "^\\s-*\\(classMethod:\\|rawClassMethod:\\)\\s-+"
              "\\([a-zA-Z_][a-zA-Z0-9_:]*\\)")
     2)
    ("Classes"
     "^\\([A-Z][a-zA-Z0-9_]*\\)\\s-+subclass:"
     1)
    ("Traits"
     "^\\([A-Z][a-zA-Z0-9_]*\\)\\s-+trait"
     1))
  "Imenu generic expression for `trashtalk-mode'.")

;;; Mode Definition

;;;###autoload
(define-derived-mode trashtalk-mode prog-mode "Trashtalk"
  "Major mode for editing Trashtalk files.

Trashtalk is a Smalltalk-inspired DSL that compiles to Bash.

\\{trashtalk-mode-map}"
  :syntax-table trashtalk-mode-syntax-table
  :group 'trashtalk

  ;; Font lock
  (setq-local font-lock-defaults '(trashtalk-font-lock-keywords))

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  ;; Indentation
  (setq-local indent-line-function #'trashtalk-indent-line)
  (setq-local indent-tabs-mode nil)

  ;; Imenu
  (setq-local imenu-generic-expression trashtalk-imenu-generic-expression)

  ;; Electric pairs for brackets
  (setq-local electric-pair-pairs '((?\[ . ?\])
                                     (?\( . ?\))
                                     (?\{ . ?\})
                                     (?\" . ?\")
                                     (?\' . ?\'))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.trash\\'" . trashtalk-mode))

(provide 'trashtalk-mode)

;;; trashtalk-mode.el ends here
