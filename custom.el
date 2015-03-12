(require-package 'molokai-theme)

;;; --------------- Don't touch the auto stuff :-p ---------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(ansi-term-color-vector
   [unspecified "#383830" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f5f4f1"])
 '(custom-enabled-themes (quote (molokai)))
 '(custom-safe-themes
   (quote
	(default)))
 '(deft-extension "md")
 '(deft-text-mode (quote markdown-mode))
 '(omnisharp-imenu-support nil)
 '(omnisharp-server-executable-path nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 140 :width normal :family "Consolas")))))
