;;; Apply any OS Local Settings First
(setq os-local-settings (concat user-emacs-directory "oslocal.el"))
(load os-local-settings)

;;; Packaging setup
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; Load themes from the themes/ directory
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist (theme-dir (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p theme-dir)
    (add-to-list 'custom-theme-load-path path)))

;;; Set the cursor shape
(setq-default cursor-type '(bar . 3))
(blink-cursor-mode -1)

;;; Load settings done with custom, do this early so we can depend on the font
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Modify whatever theme has been loaded to get rid fo the 3d modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

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
(defun get-default-height ()
  (/ (- (display-pixel-height) 120)
     (frame-char-height)))

(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))

;;; We like line numbers, really we do
(global-linum-mode 1)

;;; Use the `whitespace` module to highlight bad whitespace
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing empty space-before-tab))
(setq whitespace-trailing-regexp "\\>[^\t \n]*\\([ \t]+\\)$")
(global-whitespace-mode t)

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

;; Re-Enable Disabled Commands
(put 'upcase-region 'disabled nil)

;;; -------------------- Mode-specific hooks ---------------------------

(add-hook 'fundamental-mode-hook 'four-space-tabs)
(add-hook 'mustache-mode 'four-space-tabs)
(add-hook 'c-mode-hook
	  (lambda()
	    (four-space-tabs)
	    (c-set-style "bsd")
	    (setq c-basic-offset 4)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (four-space-tabs)
	    (setq python-indent 4)))
