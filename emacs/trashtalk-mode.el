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

(defcustom trashtalk-repl-socket "/tmp/trashtalk-repl.sock"
  "Path to the Trashtalk REPL server socket."
  :type 'string
  :group 'trashtalk)

(defcustom trashtalk-repl-timeout 5
  "Timeout in seconds for REPL commands."
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

;;; REPL Connection

(defvar trashtalk-repl-process nil
  "The network process connected to the Trashtalk REPL server.")

(defvar trashtalk-repl-response nil
  "Accumulator for REPL response data.")

(defvar trashtalk-repl-response-complete nil
  "Flag indicating response is complete.")

(defun trashtalk-repl-filter (proc string)
  "Process filter for REPL responses."
  (setq trashtalk-repl-response (concat trashtalk-repl-response string))
  (when (string-match "\n" string)
    (setq trashtalk-repl-response-complete t)))

(defun trashtalk-repl-connect ()
  "Connect to the Trashtalk REPL server."
  (interactive)
  (when (and trashtalk-repl-process
             (process-live-p trashtalk-repl-process))
    (delete-process trashtalk-repl-process))
  (condition-case err
      (progn
        (setq trashtalk-repl-process
              (make-network-process
               :name "trashtalk-repl"
               :remote trashtalk-repl-socket
               :filter #'trashtalk-repl-filter
               :sentinel (lambda (proc event)
                           (message "Trashtalk REPL: %s" (string-trim event)))))
        (message "Connected to Trashtalk REPL at %s" trashtalk-repl-socket))
    (error
     (setq trashtalk-repl-process nil)
     (error "Failed to connect to REPL: %s" (error-message-string err)))))

(defun trashtalk-repl-disconnect ()
  "Disconnect from the Trashtalk REPL server."
  (interactive)
  (when (and trashtalk-repl-process
             (process-live-p trashtalk-repl-process))
    (delete-process trashtalk-repl-process)
    (setq trashtalk-repl-process nil)
    (message "Disconnected from Trashtalk REPL")))

(defun trashtalk-repl-connected-p ()
  "Return t if connected to the REPL server."
  (and trashtalk-repl-process
       (process-live-p trashtalk-repl-process)))

(defun trashtalk-repl-ensure-connected ()
  "Ensure we're connected to the REPL, connecting if necessary."
  (unless (trashtalk-repl-connected-p)
    (trashtalk-repl-connect)))

(defun trashtalk-repl-send (command)
  "Send COMMAND to the REPL and return the response."
  (trashtalk-repl-ensure-connected)
  (setq trashtalk-repl-response ""
        trashtalk-repl-response-complete nil)
  (process-send-string trashtalk-repl-process (concat command "\n"))
  ;; Wait for response
  (let ((timeout-time (+ (float-time) trashtalk-repl-timeout)))
    (while (and (not trashtalk-repl-response-complete)
                (< (float-time) timeout-time))
      (accept-process-output trashtalk-repl-process 0.1)))
  (if trashtalk-repl-response-complete
      (string-trim trashtalk-repl-response)
    (error "REPL timeout waiting for response")))

(defun trashtalk-repl-parse-response (response)
  "Parse RESPONSE into (status . payload).
Response format: STATUS:payload"
  (if (string-match "^\\([A-Z]+\\):\\(.*\\)" response)
      (cons (match-string 1 response)
            ;; Convert unit separator back to newlines
            (replace-regexp-in-string "\x1f" "\n" (match-string 2 response)))
    (cons "ERROR" response)))

;;; REPL Interactive Commands

(defun trashtalk-repl-ping ()
  "Ping the REPL server to test connection."
  (interactive)
  (let* ((response (trashtalk-repl-send "PING"))
         (parsed (trashtalk-repl-parse-response response)))
    (if (string= (car parsed) "OK")
        (message "REPL: %s" (cdr parsed))
      (message "REPL error: %s" (cdr parsed)))))

(defun trashtalk-eval-region (start end)
  "Evaluate the region from START to END in the REPL."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         ;; Base64 encode multiline code
         (encoded (if (string-match "\n" code)
                      (concat "BASE64:" (base64-encode-string code t))
                    code))
         (response (trashtalk-repl-send (concat "EVAL:" encoded)))
         (parsed (trashtalk-repl-parse-response response)))
    (if (string= (car parsed) "OK")
        (message "=> %s" (cdr parsed))
      (message "Error: %s" (cdr parsed)))))

(defun trashtalk-eval-line ()
  "Evaluate the current line in the REPL."
  (interactive)
  (trashtalk-eval-region (line-beginning-position) (line-end-position)))

(defun trashtalk-eval-buffer ()
  "Evaluate the entire buffer in the REPL."
  (interactive)
  (trashtalk-eval-region (point-min) (point-max)))

(defun trashtalk-eval-defun ()
  "Evaluate the method definition at point."
  (interactive)
  (save-excursion
    (let (start end)
      ;; Find method start
      (end-of-line)
      (unless (re-search-backward "^\\s-*\\(raw\\)?\\(class\\)?[Mm]ethod:" nil t)
        (error "Not in a method definition"))
      (setq start (line-beginning-position))
      ;; Find matching ]
      (goto-char start)
      (re-search-forward "\\[" nil t)
      (backward-char)
      (forward-sexp)
      (setq end (point))
      (trashtalk-eval-region start end))))

(defun trashtalk-info-at-point ()
  "Get info about the class or instance at point."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (response (trashtalk-repl-send (concat "INFO:" symbol)))
         (parsed (trashtalk-repl-parse-response response)))
    (if (string= (car parsed) "INFO")
        (with-output-to-temp-buffer "*Trashtalk Info*"
          (princ (cdr parsed)))
      (message "Error: %s" (cdr parsed)))))

(defun trashtalk-methods-for-class (class-name)
  "List methods for CLASS-NAME."
  (interactive
   (list (read-string "Class: " (thing-at-point 'symbol t))))
  (let* ((response (trashtalk-repl-send (concat "METHODS:" class-name)))
         (parsed (trashtalk-repl-parse-response response)))
    (if (string= (car parsed) "METHODS")
        (with-output-to-temp-buffer "*Trashtalk Methods*"
          (princ (format "Methods for %s:\n\n" class-name))
          (princ (cdr parsed)))
      (message "Error: %s" (cdr parsed)))))

(defun trashtalk-reload-class (class-name)
  "Reload (recompile and re-source) CLASS-NAME."
  (interactive
   (list (read-string "Reload class: "
                      (save-excursion
                        (goto-char (point-min))
                        (if (re-search-forward "^\\([A-Z][a-zA-Z0-9_]*\\)\\s-+subclass:" nil t)
                            (match-string 1)
                          "")))))
  (let* ((response (trashtalk-repl-send (concat "RELOAD:" class-name)))
         (parsed (trashtalk-repl-parse-response response)))
    (if (string= (car parsed) "OK")
        (message "Reloaded: %s" class-name)
      (message "Reload error: %s" (cdr parsed)))))

(defun trashtalk-reload-current-file ()
  "Reload the class defined in the current file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\([A-Z][a-zA-Z0-9_]*\\)\\s-+subclass:" nil t)
        (trashtalk-reload-class (match-string 1))
      (error "No class definition found in buffer"))))

;;; Keymap

(defvar trashtalk-mode-map
  (let ((map (make-sparse-keymap)))
    ;; REPL commands
    (define-key map (kbd "C-c C-c") #'trashtalk-eval-defun)
    (define-key map (kbd "C-c C-r") #'trashtalk-eval-region)
    (define-key map (kbd "C-c C-b") #'trashtalk-eval-buffer)
    (define-key map (kbd "C-c C-l") #'trashtalk-eval-line)
    (define-key map (kbd "C-c C-k") #'trashtalk-reload-current-file)
    (define-key map (kbd "C-c C-z") #'trashtalk-repl-connect)
    (define-key map (kbd "C-c C-i") #'trashtalk-info-at-point)
    (define-key map (kbd "C-c C-m") #'trashtalk-methods-for-class)
    map)
  "Keymap for `trashtalk-mode'.")

;;; Mode Definition

;;;###autoload
(define-derived-mode trashtalk-mode prog-mode "Trashtalk"
  "Major mode for editing Trashtalk files.

Trashtalk is a Smalltalk-inspired DSL that compiles to Bash.

REPL Integration:
  Start the server in a terminal: @ ReplServer start
  Then connect from Emacs with C-c C-z.

Key bindings:
\\<trashtalk-mode-map>
  \\[trashtalk-eval-defun]     Evaluate method at point
  \\[trashtalk-eval-region]    Evaluate region
  \\[trashtalk-eval-buffer]    Evaluate buffer
  \\[trashtalk-eval-line]      Evaluate current line
  \\[trashtalk-reload-current-file] Reload class in current file
  \\[trashtalk-repl-connect]   Connect to REPL server
  \\[trashtalk-info-at-point]  Info for symbol at point
  \\[trashtalk-methods-for-class] List methods for class

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
