;;; oslocal-win.el --- Windows/Cygwin local settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Local settings for Windows/Cygwin environments.
;; Only loaded when running on Windows with Cygwin.

;;; Code:

(add-to-list 'exec-path "c:/tools/cygwin/bin/")

(use-package cygwin-mount
  :ensure t
  :config (cygwin-mount-activate))

(provide 'oslocal-win)
;;; oslocal-win.el ends here
