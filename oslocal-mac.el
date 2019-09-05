;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none
	  ns-function-modifier 'hyper)

(setq powerline-image-apple-rgb t)
(setq ispell-program-name "aspell")

;; Check for `omnisharp`
(if (file-exists-p "/Users/willspeak/omnisharp-osx/run")
	(setq omnisharp-expected-server-version "1.34.2"
		  omnisharp-server-executable-path "/Users/willspeak/omnisharp-osx/run"))

;; Fixup the env variables. Neede if launched from Dock or Finder
(mapcar (lambda (env)
		  ; grab the parts of the environment variable
		  (let* ((env-line (split-string env "="))
				 (env-name (car env-line))
				 (env-value (cadr env-line)))
			(setenv env-name env-value)
			(if (string= "PATH" env-name)
				(setq exec-path (split-string env-value ":")))))
		; Run the `env` command and take each line to process
		(split-string (shell-command-to-string ". ~/.profile; env")
					  "\n"))

;; Because this is what I use on windows
(global-set-key (kbd "M-<up>") 'toggle-frame-fullscreen)

;; Delete the colour list that's broken
(delete-file "~/Library/Colors/Emacs.clr")
