;; any local settings needed

(let ((oslocal-filename (cond ((eq system-type 'windows-nt) "oslocal-win.el")
							 ((eq system-type 'darwin) "oslocal-mac.el"))))
  (load (concat user-emacs-directory oslocal-filename)))
