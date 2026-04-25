;;; ------------------- Feersum Prototyping -------------------------

;; ;; Global so eglot can see it when building initialize capabilities.
;; (setq eglot-semantic-token-faces
;;       '(("comment"  . font-lock-comment-face)
;;         ("keyword"  . font-lock-keyword-face)
;;         ("number"   . font-lock-number-face)
;;         ("operator" . font-lock-operator-face)
;;         ("string"   . font-lock-string-face)
;;         ("variable" . font-lock-variable-name-face)))

(define-derived-mode feersum-mode prog-mode "Feersum"
  "Major mode for Scheme via the Feersum language server."
  (setq-local font-lock-defaults '((nil) t))) ; disable built-in keywords

(add-to-list 'auto-mode-alist '("\\.scm\\'" . feersum-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . feersum-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(feersum-mode
                 "dotnet"
                 "/home/will/Repositories/feersum/src/Feersum.LanguageServer/bin/Release/net10.0/Feersum.LanguageServer.dll")))

(add-hook 'feersum-mode-hook #'eglot-ensure)

