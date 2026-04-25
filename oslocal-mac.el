;;; oslocal-mac -- macOS Specific Initialisation -*- lexical-binding: t -*-

;;; commentary:
;; Mac settings.  This generally resolves around ensuring that macOS keys line
;; up with what I'm expecting.

;;; code:


;;; I prefer cmd key for meta
(setq-default mac-option-key-is-meta nil
			  mac-command-key-is-meta t
			  mac-command-modifier 'meta
			  mac-option-modifier 'none
			  ns-function-modifier 'hyper)

(setq-default powerline-image-apple-rgb t)
(setq-default ispell-program-name "aspell")

;; Check for `omnisharp`
(if (file-exists-p "/Users/willspeak/omnisharp-osx/run")
	(setq-default omnisharp-expected-server-version "1.34.2"
				  omnisharp-server-executable-path "/Users/willspeak/omnisharp-osx/run"))

;; Fixup the env variables. Neede if launched from Dock or Finder
(dolist (env (split-string (shell-command-to-string ". ~/.profile; env") "\n"))
  (let* ((env-line  (split-string env "="))
         (env-name  (car env-line))
         (env-value (cadr env-line)))
    (setenv env-name env-value)
    (when (string= "PATH" env-name)
      (setq exec-path (split-string env-value ":")))))

;; Because this is what I use on windows
(global-set-key (kbd "M-<up>") 'toggle-frame-fullscreen)

;; Delete the colour list that's broken
(delete-file "~/Library/Colors/Emacs.clr")

(provide 'oslocal-mac)
;;; oslocal-mac.el ends here
