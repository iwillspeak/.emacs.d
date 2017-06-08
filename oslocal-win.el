;; any local settings needed

(setq exec-path (cons "c:/tools/cygwin/bin/" exec-path))

;; make sure we can deal with Cygwin paths if we are given them
(use-package cygwin-mount
  :ensure t
  :if (getenv "CYGWIN")
  :config (cygwin-mount-activate))
