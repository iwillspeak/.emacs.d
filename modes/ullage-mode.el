;; (defun hello-world-special-setup()
;;  (make-local-variable 'foo-indent-offset)
;;   (set (make-local-variable 'indent-line-function) 'foo-indent-line))

;; (defvar foo-indent-offset 4
;;   "*Indentation offset for `foo-mode'.")

;; (defun foo-indent-line ()
;;   "Indent current line for `foo-mode'."
;;   (interactive)
;;   (let ((indent-col 0))
;;     (save-excursion
;;       (beginning-of-line)
;;       (condition-case nil
;;           (while t
;;             (backward-up-list 1)
;;             (when (looking-at "fn")
;;               (setq indent-col (+ indent-col foo-indent-offset))))
;;         (error nil)))
;;     (save-excursion
;;       (back-to-indentation)
;;       (when (and (looking-at "end") (>= indent-col foo-indent-offset))
;;         (setq indent-col (- indent-col foo-indent-offset))))
;;     (indent-line-to indent-col)))


;; (get-buffer-create "*foo*")

;; Defines a simple generic mode to hilight ullage
(define-generic-mode 'ullage-mode
  ;; Comment start marker, we just do single line comments for now
  '("#")
  ;; Keywords
  '("while" "until" "end" "let" "var" "if" "unless" "else")
  ;; Extra font locks, this is for labels and pseudo operators
  '((":\s*\\<\\([_[:alpha:]][_[:alnum:]]*\\)\\>" 1 'font-lock-type-face)
	("\\<\\(fn\\)\\>\s+\\<\\([_[:alpha:]][_[:alnum:]]*\\)\\>"
	 (1 'font-lock-keyword-face)
	 (2 'font-lock-function-name-face))
	;; ("=" . 'font-lock-builtin-face)
	("\\bprint\\b" . 'font-lock-builtin-face)
	("[0-9]+" . 'font-lock-constant-face)
	("#" . 'font-lock-comment-delimiter-face))
  ;; Lasm is where it's at
  '("\\.ulg\\'")
  'nil ;; '(hello-world-special-setup)
  ;; Docstring, says it all realy
  "A simple mode for editing ullage files")
