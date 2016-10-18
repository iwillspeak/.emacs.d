;; Extract MSMQ Packet
;;
;; This command can be run on a buffer containing a MSMQ packet copied
;; from the hex-dump screen. It wil extract the ascii data from the
;; packet.
(fset 'extract-msmq-packet
   (lambda (&optional arg)
	 "Extract the packet from an MSMQ Dump"
	 (interactive "p")
	 (kmacro-exec-ring-item
	  (quote ([134217788 19 55 66 1 67108896 134217788 backspace
	 134217765 46 46 return return 33 134217790 backspace 10 24
	 104 134217848 109 99 47 101 100 105 116 32 108 105 110 101
	 115 return 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5
	 4 return] 0 "%d")) arg)))

;; Prettyfy Json
;;
;; Quickly format json with python's `json.tool` module.
(fset 'prettify-json
   (lambda (&optional arg)
	 "Nicely format JSON"
	 (interactive "p")
	 (kmacro-exec-ring-item
	  (quote ([134217852 112 121 116 104 111 110 32 45 109 106
	  115 111 110 46 44 116 111 backspace backspace backspace 116
	  111 111 108 return] 0 "%d")) arg)))
