;;; init.el --- My custom Emacs configuration -*- lexical-binding: t; -*-

;;; commentary:

;; A `use-package' based Emacs configuration that has grown over the
;; years.  At one point it worked on Mac, Windows, and Linux but these
;; days I only really *nix.

;;; code:

;;; --------------------- Initial Settings ----------------------------

;; TRAMP settings.  Start with these as other modes might want to talk
;; over TRAMP if we're called to edit a remote file.
(setq-default tramp-default-method "ssh")
(setq-default tramp-terminal-type "tramp")

;; TLS priorities
(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(defun do-install ()
  "Thunk for `use-package' and `bind-key' bootstrap."
  (package-install 'bind-key)
  (package-install 'use-package))

;; Use `use-package`
(eval-when-compile
    (unless (package-installed-p 'use-package)
      (condition-case nil
	  (do-install)
	(error
	 (package-refresh-contents)
	 (do-install))))
    (require 'use-package))

;; Next apply any OS Local Settings
(defvar os-local-settings (concat user-emacs-directory "oslocal.el"))
(load os-local-settings)

;; Load themes from the themes/ directory
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(when (file-directory-p custom-theme-directory)
  (dolist (theme-dir (directory-files custom-theme-directory t "\\w+"))
	(when (file-directory-p theme-dir)
	  (add-to-list 'custom-theme-load-path theme-dir))))

;; Choose a Font
(cond ((find-font (font-spec :family "Menlo"))
	   (set-frame-font (font-spec :size 14.0 :family "Menlo") nil t))
	  ((find-font (font-spec :family "Noto Sans Mono"))
	   (set-frame-font (font-spec :size 13.0 :family "Noto Sans Mono" :separator-scale 0.5) nil t))
	  ((find-font (font-spec :family "Consolas"))
	   (set-frame-font (font-spec :size 14.0 :family "Consolas") nil t)))

;; Load 'look and feel' packages
(use-package powerline
  :ensure t
  :defer t)
(use-package night-owl-theme
  :ensure t
  :config (powerline-default-theme))
(use-package dracula-theme
  :ensure t
  :config (powerline-default-theme))

;; Load settings done with custom, do this early so we can depend on the font
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
 
;; Allow modes to be stored in a subdirectory and automatically loaded
(defvar custom-mode-directory (concat user-emacs-directory "modes"))
(when (file-directory-p custom-mode-directory)
  (dolist (mode-file
		   (directory-files custom-mode-directory t "\\-mode\\.el$" t))
	(load-file mode-file)))

;; Load in any custom keyboard macros
(defvar custom-macros-directory (concat user-emacs-directory "macros"))
(when (file-directory-p custom-macros-directory)
  (dolist (macros-file
		   (directory-files custom-macros-directory t "\\.el$" t))
	(load-file macros-file)))
 
;; Nice size for the default window
(defvar frame-y-padding
	 (if (eq system-type 'windows-nt)
		 160
	   120))
(defun get-default-height ()
  "Get the default window height."
  (/ (- (display-pixel-height) frame-y-padding)
     (frame-char-height)))
(defun get-min-width ()
  "Get the default window width."
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
(defvar custom-backup-directory (concat user-emacs-directory "backups"))
(setq backup-directory-alist
	  `(("." . ,(expand-file-name custom-backup-directory))))
(setq-default vc-macke-backup-files t)
 
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
(setq-default ediff-split-window-function 'split-window-horizontally)

;; Built-in line numbers (Emacs 26+)
(setq-default display-line-numbers-type t)  ; absolute numbers; use 'relative or 'visual if preferred

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'fundamental-mode-hook #'display-line-numbers-mode)

;;; ------------------ Load and Set Up All Dem  Packages ------------------

;; Diminish modes that we don't want cluttering up the powerline
(use-package diminish
  :ensure t
  :config (diminish 'visual-line-mode))

;; Writeroom provides a focus mode for single document typing
(use-package writeroom-mode
  :ensure t
  :defer t
  :bind (([f6] . writeroom-mode)))

;; Corfu for Completion
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)       ; enable everywhere
  (corfu-history-mode)      ; remember recently used candidates
  (corfu-popupinfo-mode)    ; show docs in a side popup (like VS Code's detail panel)
  :custom
  (corfu-auto t)            ; trigger automatically as you type (VS Code default)
  (corfu-auto-delay 0.2)    ; seconds before popup appears; VS Code is ~0.1-0.3
  (corfu-auto-prefix 2)     ; minimum characters before triggering
  (corfu-count 10)          ; number of candidates shown
  (corfu-cycle t)           ; wrap around at top/bottom of list
  (corfu-preselect 'first)  ; pre-select first candidate, like VS Code
  (corfu-on-exact-match nil); don't auto-insert on single match — let you confirm
  (corfu-quit-no-match t)   ; dismiss popup if nothing matches
  (corfu-popupinfo-delay '(0.3 . 0.1)) ; doc popup: (initial . subsequent) delay
  ;; :bind (:map corfu-map
  ;;             ("TAB"     . corfu-next)     ; Tab cycles like VS Code
  ;;             ([tab]     . corfu-next)
  ;;             ("S-TAB"   . corfu-previous)
  ;;             ([backtab] . corfu-previous)
  ;;             ("RET"     . corfu-insert)   ; Enter confirms
  ;;             ("M-d"     . corfu-popupinfo-toggle)) ; toggle doc panel
  :diminish corfu-mode)

;; (use-package vertico
;;   :ensure t
;;   :init
;;   (vertico-mode))

;; Adds annotations (type info, doc strings) alongside candidates
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

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
  :diminish whitespace-mode
  :config
  (global-whitespace-mode t))

;; Whitespace chars
(use-package leerzeichen
  :ensure t
  :commands leerzeichen-mode)
 
;; Automatically Paired Braces
(electric-pair-mode 1)

;; ─── C# ───────────────────────────────────────────────────────────────────────

(use-package csharp-mode
  :ensure t
  :diminish eldoc-mode
  :mode "\\.\\(cake\\|cs\\)$"
  :init
  (add-hook 'csharp-mode-hook
            (lambda ()
              (c-set-offset 'arglist-intro '+)))
  :hook
  (csharp-mode . eglot-ensure))

;; ─── F# ───────────────────────────────────────────────────────────────────────

(use-package fsharp-mode
  :ensure t
  :defer t
  :mode "\\.fs[ix]?$"
  :init
  (add-hook 'fsharp-mode-hook
            (lambda ()
              (kill-local-variable 'compile-command)))
  :hook
  (fsharp-mode . eglot-ensure))

(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode
  :config
  ;; Let eglot-fsharp manage fsautocomplete rather than a manual install
  (setq eglot-fsharp-server-install-dir nil))

;; ─── .NET test helpers ────────────────────────────────────────────────────────

(defun my/dotnet-test-at-point ()
  "Run dotnet test filtered to the symbol at point."
  (interactive)
  (compile (format "dotnet test --filter %s" (thing-at-point 'symbol t))))

(defun my/dotnet-test-buffer ()
  "Run `dotnet test' in the current project."
  (interactive)
  (compile "dotnet test"))

;; ─── Eglot (LSP) ──────────────────────────────────────────────────────────────
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c r"   . eglot-rename)
              ("<f2>"    . eglot-rename)
              ("C-."     . eglot-code-actions)
              ("M-."     . xref-find-definitions)
              ("<f12>"   . xref-find-definitions)
              ("M-,"     . xref-find-references)
              ("S-<f12>" . xref-find-references)
              ("C-c u"   . my/dotnet-test-at-point)
              ("C-c U"   . my/dotnet-test-buffer))
  :config
  (add-to-list 'eglot-server-programs
               '(csharp-mode . ("omnisharp" "-lsp")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(fsharp-mode . ("fsautocomplete" "--adaptive-lsp-server-enabled")))
  )

;; Work around Eglot calling into Emacs 30's project.el on paths that
;; sometimes trip a nil/string bug during VC project discovery.
(with-eval-after-load 'eglot
  (defun my/eglot-current-project-safe (orig)
    (condition-case err
        (funcall orig)
      (wrong-type-argument
       (if (and (eq (cadr err) 'stringp)
                (null (caddr err)))
           `(transient . ,(expand-file-name
                           (or (ignore-errors (file-truename default-directory))
                               default-directory)))
         (signal (car err) (cdr err))))))
  (advice-add 'eglot--current-project :around #'my/eglot-current-project-safe))

;; Work around Emacs 30 project.el crashing on symlinked paths when
;; VC root discovery fails and `project--vc-merge-submodules-p' sees nil.
(with-eval-after-load 'project
  (defun my/project-try-vc-use-truename (orig dir)
    (funcall orig
             (if dir
                 (or (ignore-errors (file-truename dir))
                     dir)
               dir)))
  (advice-add 'project-try-vc :around #'my/project-try-vc-use-truename)

  (defun my/project-vc-merge-submodules-p-safe (orig dir)
    (if dir
        (funcall orig dir)
      project-vc-merge-submodules))
  (advice-add 'project--vc-merge-submodules-p
              :around #'my/project-vc-merge-submodules-p-safe))

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
(use-package git-modes
  :ensure t
  :mode "\\.(gitignore|gitconfig)")
(use-package flyspell
  :bind (:map flyspell-mode-map
			  ("C-;" . nil)))
(use-package markdown-mode
  :ensure t
  :mode "\\.m(d|arkdown)"
  :init (add-hook 'markdown-mode-hook 'flyspell-mode))
(use-package ruby-mode
  :mode "\\(Rakefile\\)\\|\\(\\.rb\\)\\$")

(use-package rust-mode
  :ensure t
  :mode "\\.rs"
  :diminish eldoc-mode
  :bind (:map rust-mode-map
			  ("C-c b" . rust-compile))
  :init (add-hook 'rust-mode-hook
		  (lambda ()
		    (setq indent-tabs-mode nil)
		    (kill-local-variable 'compile-command))))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))
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
  "Move the file this buffer is visiting."
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
  "Use four-space tabs for indentation."
  (interactive)
  (setq tab-width 4)
  (setq indent-tabs-mode t))
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq tab-stop-list (number-sequence 4 200 4))
 
(defun c-like-indent()
  "Use c-style indetation."
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
			(setq-local python-indent 4)))

;; Treat bat files as dos files. Not sure why this isn't default...
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.props$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.nuspec$" . xml-mode))

;;; -------------------------- Final Settings --------------------------

(provide 'init)
;;; init.el ends here
