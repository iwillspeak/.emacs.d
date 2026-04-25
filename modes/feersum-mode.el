;;; feersum-mode -- Feersum Prototyping -*- lexical-binding: t -*-

;;; commentary:
;; A simple mode for editing Feersum Scheme files.  Provides a basic
;; LSP experience

;;; code:

(define-derived-mode feersum-mode prog-mode "Feersum"
  "Major mode for Scheme via the Feersum language server.") ; disable built-in keywords

(add-to-list 'auto-mode-alist '("\\.scm\\'" . feersum-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . feersum-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(feersum-mode
                 "dotnet"
				 "dnx"
				 "Feersum.LanguageServer")))
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(feersum-mode
;;                  "dotnet"
;;                  "/home/will/Repositories/feersum/src/Feersum.LanguageServer/bin/Release/net10.0/Feersum.LanguageServer.dll")))

(add-hook 'feersum-mode-hook #'eglot-ensure)

(provide 'feersum-mode)
;;; feersum-mode.el ends here
