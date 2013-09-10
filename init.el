;;; --------------------- Initial Settings ----------------------------
 
;; Apply any OS Local Settings First
(setq os-local-settings (concat user-emacs-directory "oslocal.el"))
(load os-local-settings)
 
;; Next up enable the package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
 
;; This little function ensures that we have the packages installed
(defun require-package (packagename)
  (unless (package-installed-p packagename)
    (package-install packagename)))
 
;; Load themes from the themes/ directory
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(dolist (theme-dir (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p theme-dir)
    (add-to-list 'custom-theme-load-path path)))
 
;; Load settings done with custom, do this early so we can depend on the font
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
 
;; Allow modes to be stored in a subdirectory and automatically loaded
(setq custom-mode-directory (concat user-emacs-directory "modes"))
(when (file-directory-p custom-mode-directory)
  (dolist (mode-file
		   (directory-files custom-mode-directory t "\\-mode\\.el$" t))
	(load-file mode-file)))
 
;; Nice size for the default window
(defun get-default-height ()
  (/ (- (display-pixel-height) 120)
     (frame-char-height)))
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
 
;; Set the cursor shape, I like the chunky cursor from my phone... :-p
(setq-default cursor-type '(bar . 3))
(blink-cursor-mode -1)
 
;; Modify whatever theme has been loaded to get rid fo the 3d modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
 
;; Get rid of some of the widow clutter
(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
 
 
;;; --------------------- Set Up Builtin Modes ---------------------------
 
;; Default to looking in the home directory, not sure why emacs doesn't on win
(setq default-directory "~/")
 
;; Nice wrapping of things
(global-visual-line-mode t)
 
;; We like line numbers, really we do
(global-linum-mode 1)
 
;; Overtype in selections
(delete-selection-mode 1)
 
;; Stop the annoying noises
(setq ring-bell-function (lambda ()))
 
;; Re-Enable Disabled Commands
(put 'upcase-region 'disabled nil)
 
;; Nice Highlighting for parens
(show-paren-mode)
 
;; Auto refresh buffers, all of them
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
 
;; Control back up files
(setq custom-backup-directory (concat user-emacs-directory "backups"))
(setq backup-direcotry-alist
	  `(("." . ,(expand-file-name custom-backup-directory))))
(setq vc-macke-backup-files t)

;; Nicer keybinding for undo
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-;") 'undo)
 
;; Join lines togeter easily
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))
 
 
;;; ------------------ Load and Set Up All Dem  Packages ------------------
 
;; Bind expand region, pretty useful key combination
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)
 
;; Multiple Cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-=") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
 
;; Use the `whitespace` module to highlight bad whitespace
(require-package 'whitespace)
(setq whitespace-style '(face lines-tail trailing empty space-before-tab))
(setq whitespace-trailing-regexp "\\>[^\t \n]*\\([ \t]+\\)$")
(global-whitespace-mode t)
 
;; Nice modeline
(require-package 'powerline)
(powerline-default-theme)
 
;; Automatically Paired Braces
(require-package 'autopair)
(autopair-global-mode)
 
 
;;; -------------------- Mode-specific hooks ---------------------------
 
;; Four space tabs
(defun four-space-tabs()
  (interactive)
  (setq tab-width 4)
  (setq indent-tabs-mode t))
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq tab-stop-list (number-sequence 4 200 4))
 
(defun c-like-indent()
  (four-space-tabs)
  (c-set-style "bsd")
  (setq c-basic-offset 4))
 
;; Attach hooks to the modes I'm interested in
(add-hook 'fundamental-mode-hook 'four-space-tabs)
(add-hook 'mustache-mode 'four-space-tabs)
(add-hook 'c-mode-hook 'c-like-indent)
(add-hook 'c++-mode-hook 'c-like-indent)
(add-hook 'python-mode-hook
		  (lambda ()
			(four-space-tabs)
			(setq indent-tabs-mode nil)
			(setq python-indent 4)))
 
 
;;; -------------------------- Final Settings --------------------------
 
;; Diminish modes that we don't want cluttering up the powerline
(require-package 'diminish)
(diminish 'global-whitespace-mode)
(diminish 'whitespace-mode)
(diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)
(diminish 'autopair-mode)
