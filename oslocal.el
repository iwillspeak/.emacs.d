;; any local settings needed

(let ((oslocal-filename (cond ((eq system-type 'windows-nt) "oslocal-win.el")
			      ((eq system-type 'darwin) "oslocal-mac.el")
			      ((eq system-type 'gnu/linux) "oslocal-linux.el"))))
  (load (concat user-emacs-directory oslocal-filename)))
