;;; feersum-mode.el --- Feersum Scheme major mode -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple major mode for editing Feersum Scheme files.
;; Provides syntax highlighting via scheme-mode and an eglot LSP integration.

;;; Code:

(require 'eglot)
(require 'scheme)

;; ─── Customization ────────────────────────────────────────────────────────────

(defgroup feersum nil
  "Support for the Feersum Scheme language server."
  :group 'languages
  :prefix "feersum-")

(defcustom feersum-server-command '("dotnet" "dnx" "Feersum.LanguageServer")
  "Command to start the Feersum language server.
Can be overridden to point at a local build, e.g.:
  (setq feersum-server-command
        \\='(\"dotnet\" \"/path/to/Feersum.LanguageServer.dll\"))"
  :type '(repeat string)
  :group 'feersum)

;; ─── Mode definition ──────────────────────────────────────────────────────────

(define-derived-mode feersum-mode scheme-mode "Feersum"
  "Major mode for Scheme via the Feersum language server.")

(add-to-list 'auto-mode-alist '("\\.scm\\'" . feersum-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . feersum-mode))

;; ─── Eglot integration ────────────────────────────────────────────────────────

(add-to-list 'eglot-server-programs
             `(feersum-mode . ,(lambda (_) feersum-server-command)))

(add-hook 'feersum-mode-hook #'eglot-ensure)

(provide 'feersum-mode)
;;; feersum-mode.el ends here
