;;; --------------------- Initial Settings ----------------------------

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20015
(setq tramp-ssh-controlmaster-options "")

;; reduce startup time by raising the gc threshold
(setq gc-cons-threshold 100000000)

;; restore default threshold after 5 seconds
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))

;; First set up the package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Use `use-package`
(unless (package-installed-p 'use-package)
  (condition-case nil
      (package-install 'use-package)
    (error
     (package-refresh-contents)
     (package-install 'use-package))))
(require 'use-package)

;; Next apply any OS Local Settings
(setq os-local-settings (concat user-emacs-directory "oslocal.el"))
(load os-local-settings)

;; Load themes from the themes/ directory
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(when (file-directory-p custom-theme-directory)
  (dolist (theme-dir (directory-files custom-theme-directory t "\\w+"))
	(when (file-directory-p theme-dir)
	  (add-to-list 'custom-theme-load-path path))))

;; Choose a Font
(cond ((find-font (font-spec :family "Menlo"))
	   (set-frame-font (font-spec :size 14.0 :family "Menlo") nil t))
	  ((find-font (font-spec :family "Consolas"))
	   (set-frame-font (font-spec :size 14.0 :family "Consolas") nil t)))

;; Load 'look and feel' packages
(use-package powerline
  :ensure t
  :defer t)
(use-package nord-theme
  :ensure t
  :config (powerline-default-theme))

;; Load settings done with custom, do this early so we can depend on the font
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
 
;; Allow modes to be stored in a subdirectory and automatically loaded
(setq custom-mode-directory (concat user-emacs-directory "modes"))
(when (file-directory-p custom-mode-directory)
  (dolist (mode-file
		   (directory-files custom-mode-directory t "\\-mode\\.el$" t))
	(load-file mode-file)))

;; Load in any custom keyboard macros
(setq custom-macros-directory (concat user-emacs-directory "macros"))
(when (file-directory-p custom-macros-directory)
  (dolist (macros-file
		   (directory-files custom-macros-directory t "\\.el$" t))
	(load-file macros-file)))
 
;; Nice size for the default window
(set 'frame-y-padding
	 (if (eq system-type 'windows-nt)
		 160
	   120))
(defun get-default-height ()
  (/ (- (display-pixel-height) frame-y-padding)
     (frame-char-height)))
(defun get-min-width ()
  (/ (- (display-pixel-width) 50)
	 (frame-char-width)))
(add-to-list 'default-frame-alist (cons 'width (min 140 (get-min-width))))
(add-to-list 'default-frame-alist (cons 'height (get-default-height)))
(add-to-list 'default-frame-alist '(top . 10))
(add-to-list 'default-frame-alist '(left . 10))
 
;; Set the cursor shape, I like the chunky cursor from my phone... :-p
(setq-default cursor-type '(bar . 3))
(blink-cursor-mode -1)
 
;; Modify whatever theme has been loaded to get rid fo the 3d modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
 
;; Get rid of some of the widow clutter
(when window-system
  (set-scroll-bar-mode nil)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

;;; --------------------- Set Up Builtin Modes ---------------------------
 
;; Default to looking in the home directory, not sure why emacs doesn't on win
(if (eq system-type 'windows-nt)
	(setq default-directory "~/"))
 
;; Nice wrapping of things
(global-visual-line-mode 't)

;; Overtype in selections
(delete-selection-mode 1)
 
;; Stop the annoying noises
(setq ring-bell-function 'ignore)
 
;; Re-Enable Disabled Commands
(put 'upcase-region 'disabled nil)
 
;; Nice Highlighting for parens
(show-paren-mode)

;; Better completion of things
(icomplete-mode)
(ido-mode)
 
;; Auto refresh buffers, all of them
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
 
;; Control back up files
(setq custom-backup-directory (concat user-emacs-directory "backups"))
(setq backup-directory-alist
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

;; I pretty much never want C-Z to do what it does
(global-unset-key (kbd "C-z"))

;; Visual Studio does some things right...
(global-set-key (kbd "<f5>") 'recompile)
(setq compilation-scroll-output 'first-error)

;; Setup window splitting when diffing
(setq ediff-split-window-function 'split-window-horizontally)

;;; ------------------ Load and Set Up All Dem  Packages ------------------

;; Diminish modes that we don't want cluttering up the powerline
(use-package diminish
  :ensure t
  :config (diminish 'visual-line-mode))

;; We like line numbers, really we do
(use-package nlinum
  :ensure t
  :commands nlinum-mode
  :init (progn (add-hook 'fundamental-mode-hook 'nlinum-mode)
			   (add-hook 'prog-mode-hook 'nlinum-mode)
			   (setq nlinum-highlight-current-line t))
  :config (add-hook 'nlinum-mode-hook
					(lambda ()
					  (setq-local nlinum-format
								  (concat "%" (number-to-string
											   ;; Guesstimate number of buffer lines.
											   (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
										  "d")))))

;; Company for Completion
(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode)

;; Bind expand region, pretty useful key combination
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C-+" . er/contract-region)))

;; Ripgrep!
(use-package rg
  :ensure t
  :commands (rg-literal rg)
  :bind ("H-SPC" . rg))
 
;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :init (global-unset-key (kbd "M-<down-mouse-1>"))
  :commands mc/edit-lines
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-=" . mc/mark-all-like-this)
	 ("C-M->" . mc/skip-to-next-like-this)
	 ("C-M-<" . mc/skip-to-previous-like-this)
	 ("M-<mouse-1>" . mc/add-cursor-on-click)))

;; Use the `whitespace` module to highlight bad whitespace
(use-package whitespace
  :ensure t
  :init
  (setq whitespace-style '(face trailing empty space-before-tab))
  (setq whitespace-trailing-regexp "\\>[^\t \n]*\\([ \t]+\\)$")
  :defer 3
  :diminish global-whitespace-mode
  :config
  (global-whitespace-mode t))

;; Whitespace chars
(use-package leerzeichen
  :ensure t
  :commands leerzeichen-mode)
 
;; Automatically Paired Braces
(use-package autopair
  :ensure t
  :diminish autopair-mode
  :defer 1
  :config (autopair-global-mode))

;; Deferred HTTP to talk to Omnisharp
(use-package request-deferred
  :ensure t)

;; Omnisharp
(use-package omnisharp
  :ensure t
  :defer t
  :init (add-hook 'omnisharp-mode-hook
				  (lambda ()
					(setq-local company-backends '(company-omnisharp))
					(auto-complete-mode -1)
					(company-mode)))
  ;; :config (setq omnisharp-use-http t)
  :commands omnisharp-mode
  :bind (:map omnisharp-mode-map
			  ;; ("C-r C-r" . omnisharp-rename) collides with reverse search
			  ("C-." . omnisharp-run-code-action-refactoring)
			  ("M-." . omnisharp-go-to-definition)
			  ("M-," . omnisharp-find-usages)))
(use-package csharp-mode
  :ensure t
  :init (progn
		  (add-hook 'csharp-mode-hook 'omnisharp-mode)
		  (add-hook 'csharp-mode-hook (lambda ()
										(c-set-offset 'arglist-intro '+))))
  :mode ("\\.cake$" "\\.cs$"))

(use-package fsharp-mode
  :ensure t
  :init (add-hook 'fsharp-mode-hook (lambda ()
									  (kill-local-variable 'compile-command)))
  :mode "\\.fs[ix]?")


;; Notes Buffer Support
(use-package deft
  :ensure t
  :config (progn
			(setq deft-default-extension "md")
			(setq deft-markdown-mode-title-level 1)
			(setq deft-use-filter-string-for-filename t))
  :bind (([f8] . deft)
		 :map deft-mode-map
		 ("C-k" . deft-delete-file)))

;; Icons for *ALL* the things \o/
(use-package all-the-icons
  :ensure t
  :if window-system)

;; Trees on the side
(use-package neotree
  :ensure t
  :if window-system
  :config (progn
			(setq neo-window-fixed-size nil
				  neo-show-hidden-files t
				  neo-theme 'ascii
				  neo-banner-message nil
				  neo-create-file-auto-open t)
			(add-hook 'neotree-mode-hook
					  (lambda ()
						(hl-line-mode)
						(visual-line-mode -1)
						(setq truncate-lines t
							  tooltip-mode nil
							  show-help-function nil))))
  :bind (("C-(" . neotree-toggle)
		 :map neotree-mode-map
		 ([double-mouse-1] . neotree-change-root)))

;; Useful Modes
(use-package magit
  :ensure t
  :defer 5
  :bind (("M-s" . magit-status)))
(use-package gitconfig-mode
  :ensure t
  :mode "\\.gitconfig")
(use-package gitignore-mode
  :ensure t
  :mode "\\.gitignore")
(use-package markdown-mode
  :ensure t
  :mode "\\.m(d|arkdown)")
(use-package ruby-mode
  :mode ("Rakefile" "\\.rb"))

;; ;; Racer for Rust
;; (use-package racer
;;   :ensure t
;;   :defer t
;;   :init (progn
;; 		  (add-hook 'racer-mode-hook #'eldoc-mode)
;; 		  (add-hook 'racer-mode-hook #'company-mode))
;;   :commands racer-mode
;;   :bind (:map rust-mode-map
;; 			  ("TAB" . company-indent-or-complete-common)))
(use-package rust-mode
  :ensure t
  :mode "\\.rs"
  :diminish eldoc-mode
  :bind (:map rust-mode-map
			  ("C-c b" . rust-compile))
  :init (add-hook 'rust-mode-hook
				  (lambda ()
					(lsp)
					(kill-local-variable 'compile-command))))
(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :diminish lsp-mode
  :config
  (use-package lsp-clients))
(use-package company-lsp
  :ensure t
  :after company)
(use-package toml-mode
  :ensure t
  :mode "\\.toml")
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml")
(use-package powershell
  :ensure t
  :mode ("\\.ps1" . powershell-mode)
  :commands powershell-mode)
(use-package go-mode
  :ensure t
  :mode "\\.go$")
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))
(use-package ponylang-mode
  :ensure t
  :mode "\\.pony$")
(use-package elm-mode
  :ensure t
  :mode "\\.elm$")

;;; ---------------------- General Commands ---------------------------

(defun move-file ()
  (interactive)
  (let ((old-name (buffer-file-name)))
	(if (not (and old-name (file-exists-p old-name)))
		(message "Can't move, buffer isn't visiting a file")
	  (let ((new-name (read-file-name "New name: " old-name)))
		(rename-file old-name new-name t)
		(set-visited-file-name new-name t t)))))
(global-set-key (kbd "C-x r") 'move-file)
 
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
(add-hook 'c-mode-hook 'c-like-indent)
(add-hook 'c++-mode-hook 'c-like-indent)
(add-hook 'python-mode-hook
		  (lambda ()
			(four-space-tabs)
			(setq indent-tabs-mode nil)
			(setq python-indent 4)))

;; Treat bat files as dos files. Not sure why this isn't default...
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.props$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.nuspec$" . xml-mode))

;;; -------------------------- Final Settings --------------------------
