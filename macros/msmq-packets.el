;;; msmq-packets.el --- MSMQ packet extraction utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for working with MSMQ packets.
;; The `extract-msmq-packet' command operates on a buffer containing
;; an MSMQ packet copied from a hex-dump screen, extracting the ASCII
;; data from the packet.

;;; Code:

(defun extract-msmq-packet (&optional _arg)
  "Extract the packet from an MSMQ Dump."
  (interactive "p")
  (funcall
   (lambda ()
     (kmacro-exec-ring-item
      (quote ([134217788 19 55 66 1 67108896 134217788 backspace
               134217765 46 46 return return 33 134217790 backspace 10 24
               104 134217848 109 99 47 101 100 105 116 32 108 105 110 101
               115 return 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5
               4 return] 0 "%d"))))))


(provide 'msmq-packets)
;;; msmq-packets.el ends here
