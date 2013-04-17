(require :asdf)
;(ql:quickload 'usocket)
;(asdf:operate 'asdf:load-op :usocket)
(asdf:oos 'asdf:load-op :usocket)
;(use-package 'usocket)

; UTILITIES
(load #p"./utils.lisp")

(load #p"./server_internal.lisp")

; INITIAL DATA
(load #p"./local_config.lisp")

;initial values for internal variables
(known-nodes 'add-node *my-node-name* *my-node-address*)
(known-nodes 'add-programs *my-node-name* `(("LOCAL_SERVER" ,*my-node-port* (("NODE-SERVER" "SERVER0")))))

; main function
(defun server-main ()
	;; start server threads
	(main-socket-listener 'start)
	(periodic-sender 'start)
	(do ((command-string 
			(string-upcase (read-line))
			(string-upcase (read-line))))
		((string-equal command-string "QUIT") 
			;(main-socket-listener 'close)
			 ;(periodic-sender 'close)
			 0)
		;(format t "Read: ~A~&" command-string)
		(cond 
			((string-equal command-string "INFO")
				(known-nodes 'print-info))
			((string-equal command-string "ADDNODE") ;!!!!! OCHE BYDLOCODE
				(append *multicast-addresses* (read-line)))
			((string-equal command-string "DEBUG")
				(if known-nodes-trace
					(progn (untrace known-nodes)
						(format t "Debug off~&"))
					(progn (trace known-nodes)
						(format t "Debug on~&")))
				(setf known-nodes-trace (not known-nodes-trace)))
			((string-equal command-string "RENEW")
			 (renew-our-config))
			(t (format t "Unknown command: ~A~&" command-string)))))

;(known-nodes 'add-programs *my-node-name* '(("R_PROG2" 3245 (("RANGE-FINDER" "RT0")))))
;(known-nodes 'add-programs *my-node-name* '(("R_PROG1" 3245 (("2D-MOTION" "MT1")))))
;(known-nodes 'add-programs *my-node-name* '(("R_PROG3" 3245 (("CAMERA" "MT2")))))

(sb-ext:save-lisp-and-die "coord.exe" :toplevel #'server-main :executable t)
