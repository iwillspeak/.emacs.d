;; Prettyfy Json
(defun prettify-json ()
  "Quickly format json with `jq` or python's `json.tool' module."
  (interactive)
  (let ((start (if (use-region-p)
				  (region-beginning)
				 (buffer-end -1)))
		(end (if (use-region-p)
				 (region-end)
			   (buffer-end 1)))
		(tool (if (executable-find "jq")
				  "jq"
				"python -mjson.tool")))
	(shell-command-on-region start end tool 'insert 'replace)))
