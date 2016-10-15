(use-package moe-theme
  :ensure t
  :config (moe-theme-random-color))

;;; --------------- Don't touch the auto stuff :-p ---------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-term-color-vector
   [unspecified "#383830" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f5f4f1"] t)
 '(custom-enabled-themes (quote (moe-dark)))
 '(custom-safe-themes
   (quote
	("32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" default)))
 '(deft-directory "/Users/will/Notes")
 '(package-selected-packages
   (quote
	(moe-theme yaml-mode use-package toml-mode rust-mode powerline omnisharp neotree multiple-cursors markdown-mode magit leerzeichen gruvbox-theme gitignore-mode gitconfig-mode expand-region deft autopair)))
 '(when (not (facep (aref ansi-term-color-vector 0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
