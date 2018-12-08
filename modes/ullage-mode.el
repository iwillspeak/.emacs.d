;; Defines a simple generic mode to hilight Ullage
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
  ;; Ullage is where it's at
  '("\\.ulg\\'")
  '(ullage-mode-setup)
  ;; Docstring, says it all realy
  "A simple mode for editing Ullage files")
