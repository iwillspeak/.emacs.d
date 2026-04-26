;;; oslocal.el --- Dispatch to a local OS settings override -*- lexical-binding: t; -*-

;;; commentary:
;; any local settings needed

;;; code:

(let ((oslocal-filename (cond ((eq system-type 'windows-nt) "oslocal-win.el")
			      ((eq system-type 'darwin) "oslocal-mac.el")
			      ((eq system-type 'gnu/linux) "oslocal-linux.el"))))
  (load (concat user-emacs-directory oslocal-filename)))


(provide 'oslocal)
;;; oslocal.el ends here
