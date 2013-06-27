;;; Start the server, so we can connect clients
(server-start)

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;;; Packaging setup
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; Epxand region, should be pretty sweet
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; Four space tabs
(defun four-space-tabs()
  (interactive)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq tab-stop-list (number-sequence 4 200 4)))
(four-space-tabs)

;;; Nice wrapping of things
(global-visual-line-mode t)

;;; Nice size for the default window
(setq default-frame-alist
      '((fullscreen . fullheight) (cursor-type bar)))
(setq frame-inherited-parameters
      '(width height))
;; gonna have to hack this in it seems .. :-/
(add-hook 'after-init-hook
	  (lambda ()
	    (set-frame-parameter nil 'width 140)))

;;; We like line numbers, really we do
(global-linum-mode 1)

;;; Use the `whitespace` module to highlight bad whitespace
(require 'whitespace)
(setq whitespace-style
      '(face lines-tail trailing empty space-before-tab))
(global-whitespace-mode t)
(setq whitespace-trailing-regexp "\\b\\([    ]+\\)$")

;;; Overtype in selections
(delete-selection-mode 1)

;;; Stop the annoying noises
(setq ring-bell-function (lambda ()))

;;; Nice modeline
(require 'powerline)
(powerline-default-theme)

;;; Keybindings
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-;") 'undo)

;;; -------------------- Mode-specific hooks ---------------------------

(add-hook 'fundamental-mode-hook 'four-space-tabs)
(add-hook 'mustache-mode 'four-space-tabs)
(add-hook 'c-mode-hook
	  (lambda()
	    (four-space-tabs)
	    (c-set-style "bsd")))
(add-hook 'python-mode-hook
	  (lambda ()
	    (four-space-tabs)
	    (setq python-indent 4)))

;;; --------------- Don't touch the auto stuff :-p ---------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (monokai)))
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 130 :width normal :foundry "apple" :family "Monaco"))))
 '(mode-line ((t (:background "chartreuse" :foreground "gray10"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :weight light))))
 '(powerline-active1 ((t (:inherit mode-line :background "firebrick1" :foreground "White")))))
