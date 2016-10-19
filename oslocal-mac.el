;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Because this is what I use on windows
(global-set-key (kbd "M-<up>") 'toggle-frame-fullscreen)
