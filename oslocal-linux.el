;;; oslocal-linux -- Linux Specific Initialisation -*- lexical-binding: t -*-

;;; commentary:
;; Linux settings

;;; code:
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(provide 'oslocal-linux)
;;; oslocal-linux.el ends here
