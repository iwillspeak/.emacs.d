;; Prettyfy Json
(defun prettify-json ()
  "Quickly format json with python's `json.tool' module"
  (interactive)
  (let ((start (if (use-region-p)
				  (region-beginning)
				 (buffer-end -1)))
		(end (if (use-region-p)
				 (region-end)
			   (buffer-end 1))))
	(shell-command-on-region start end "python -mjson.tool" 'insert 'replace)))
