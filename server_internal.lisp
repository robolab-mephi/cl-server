; SERVER 

; internal variables
(defvar *my-node-name* (string-upcase (concatenate 'string "MyNode" (write-to-string (get-universal-time)))))
(defvar *my-node-address* "192.168.208.140")
(defvar *multicast-addresses* '("192.168.208.110"))
(defvar *my-node-port* 2345)

; debug action detalization:
; no --- no actions
; all --- all actions
(let ((debug-level 'no)
	(message-types '(no all)))
	(defun set-debug-level (level)
		(when (find level message-types)
			(setf debug-level level)))
	(defmacro debug-action (body)
		(when (equal debug-level 'all)
			`(eval ,body))))
			

; function descriptor 
(defstruct function-descriptor
  name)

; device pattern 
(defstruct device-pattern
;  name ; pattern name
  functions ;list of function descriptors
)


; known device patterns list
; issues: unknown devices doesn't work
; kn-devices: H(device-name):S(device-pattern)
; -device-pattern: L(function-descriptor)
; unknw-devices: L(device-name)

(let (; known devices descriptors
      (kn-devices (make-hash-table :test #'equal))
      ; unrecognized devices list
      (unknw-devices nil))
  (defun known-devices (cmd &rest args)
    (case cmd
      ;; set functions list to device  
      ;; 'add-device device-name L(functions) -> t
      ;; if device doesn't exist, it'll be created.
      (set-device-functions
       (progn
			;;; add device if nessesary
	 (when (null (nth-value 1 (gethash (car args) kn-devices)))
	   (setf (gethash (car args) kn-devices) (make-device-pattern :functions nil)))
			;;; update device functions
	 (setf (device-pattern-functions (gethash (car args) kn-devices))
	       (mapcar #'(lambda (func-name)
			   (make-function-descriptor :name func-name))
		       (remove-duplicates (cdr args) :test #'equal)))
	 (remove (car args) unknw-devices :test #'equal)
	 t))
      ;; get functions list from device
      ;; 'get-device-functions device-name -> L(function-descriptor) | nil, if device doesn't exist
      (get-device-functions
       (multiple-value-bind (device-descr is-device) (gethash (car args) kn-devices)
	 (if is-device
	     (device-pattern-functions device-descr)
	     (progn (push unknw-devices (car args))
		    nil))))
      ;; is there device with name given?
      ;; 'is-device? device-name -> t | nil; if unknown, add device to list
      (is-device?
       (if (nth-value 1 (gethash (car args) kn-devices))
	   t
	   (progn (setf unknw-devices (list (car args) unknw-devices))
		  nil)))
	;; print info about known and unknown devices
      ;; 'print-info -> t and printing
      (print-info
       (progn
	 (format t "Known devices: ~&")
	 (maphash #'(lambda (dev-name dev-descr)
		      (format t "~A: ~A~&" dev-name dev-descr))
		  kn-devices)
	 (format t "Unknown devices: ~A~&" unknw-devices)))
      (t ;;; !!! ADD exception here
       (format t "known-devices: unknown command ~A~&" cmd)
       nil))))

; program descriptor
(defstruct program
;  name ; program name
  port ; program communication port
  devices ; list of devices
)

;
(defstruct device
  type ; predefined type
  name ; name
)

; node descriptor structure
(defstruct node
;  name ; node name
  address ; node address
  programs ; programs of node
)	 

; log messages; now --- in console only
(defun add-log-message (sender text)
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format t "~A/~A/~A, ~A:~A:~A: Message from ~A: ~A~&" year month date hour minute second sender text)))

; known nodes
; issues: !!!
; kn-nodes: H(node-name): S node(address programs)
; programs: H(program-name): S program ( port devices)
; devices: L S device(type name)

(let ((kn-nodes (make-hash-table :test #'equal)))
  (defun known-nodes (cmd &rest args)
  ; Formats: 
    (case cmd
      ;; 'add-node node-name address -> t, if node already exists
      (add-node
       (if (null (nth-value 1 (gethash (car args) kn-nodes))) 
	      ;;; if node doesn't exist - add it
	   (progn (setf (gethash (car args) kn-nodes) 
			(make-node :address (cadr args) :programs (make-hash-table :test #'equal)))
		  nil)
	      ;;; if node exists, update its address
	   (progn (setf (node-address (gethash (car args) kn-nodes)) 
			(cadr args))
		  t)))
      ;; 'print-info -> t and printing
      (print-info
       (format t "Known nodes info:~&")
       (maphash #'(lambda (node-name node-descr)
		    (format t "~A: ~A~&" node-name (node-address node-descr))
		    (maphash #'(lambda (prog-name prog-devices)
				 (format t " ~A, port ~A:~&" prog-name (program-port prog-devices))
				 ;;; print program info
				 (mapcar #'(lambda (program-descr)
					     (format t "  ~A ~A~&" (device-type program-descr) (device-name program-descr)))
					 (program-devices prog-devices)))
			     (node-programs node-descr)))
		kn-nodes))
    ;; 'add-programs node-name L(program port L(device-type device-name))
      (add-programs
       (if (nth-value 1 (gethash (car args) kn-nodes))
              ;;; if there is the node, set known programs' hash of node to programs-of-node
	   (let ((programs-of-node (node-programs (gethash (car args) kn-nodes))))
	      ;;; for each (program port L(device-type device-name))
	      ;;; if this program isn't here - add it !!!device check!!!
	     (mapcar #'(lambda (prog-devices) ; prog-devices :  (program port L(device-type device-name)
			 ;;; if there is no such program in the node --- add it
			 (if (null (nth-value 1 (gethash (car prog-devices) programs-of-node)))
			     (progn
                   ;;; add list of structures "type - name" to program descriptor 
			       (format t "PORT: ~A, devices ~A ~&" (cadr prog-devices) (caddr prog-devices))
			       (setf (gethash (car prog-devices) programs-of-node) 
				     (make-program :port (cadr prog-devices)
						   :devices
						   (mapcar #'(lambda (dev-info) ;;; dev-info: (device-type device-name)
									;;; check if the device is unknown 
							       (known-devices 'is-device? (car dev-info))
									;;; create device with this type anyway
							       (make-device :type (car dev-info) :name (cadr dev-info)))
							   (caddr prog-devices))))
			       
					;;;!!! All devices added to final list. 
					;;; Should make list of unknown devices of programs and commit it when description'll be recieved.
			       t)))				       
		     (cadr args)))
	   ;;; if node doesn't exists, make warning
	   (add-log-message "(known-nodes 'add-programs ...)" 
			    (format nil "Node ~A not found" (car args)))))
      ;; 'add-own-programs (L(program L(device-type device-name)))
      (add-own-programs
       (apply #'known-nodes 'add-programs *my-node-name* args))
      ;; 'add-devices-of-program program L(device-type device-name) - our node only
      ;; !!! make updater, not setter
      (add-devices-of-program
     ;;; if there is such program
       (when (known-nodes 'is-my-program? (car args))
	 	;;; add device to it's list
	 (setf (gethash (car args) (node-programs (gethash *my-node-name* kn-nodes)))
	       (make-program :port (program-port (gethash (car args) (node-programs (gethash *my-node-name* kn-nodes))))
				:devices (mapcar #'(lambda (dev-info) ;;; dev-info: (device-type device-name)
			   ;;; check if the device is unknown 
			   (known-devices 'is-device? (car dev-info))
			   ;;; create device with this type anyway
			   (make-device :type (car dev-info) :name (cadr dev-info)))
		       (cadr args))))))
      ;; is-program-of-node? node-name program-name ->t, if the node has this program, else nil
      (is-program-of-node?
       (multiple-value-bind (node node-is-here) (gethash (car args) kn-nodes)
	 (and node-is-here (nth-value 1 (gethash (cadr args) (node-programs node))))))
      ;; is-my-program? <program-name> ->t, if our node has this program, else nil
      (is-my-program?
       (apply #'known-nodes 'is-program-of-node? *my-node-name* args))
      ;; 'get-node-programs node-name -> L(program port L(device-type device-name))
      (get-node-programs 
     ;;; get all known programs of node if it exists
       (multiple-value-bind (node-value node-is-here) (gethash (car args) kn-nodes)
	 (when node-is-here
	   (let ((res nil))
	     (maphash #'(lambda (prog-name prog-descr)
			  (push (list prog-name (program-port prog-descr) 
				      (mapcar #'(lambda (curr-device)
						  (list (device-type curr-device) (device-name curr-device)))
					      (program-devices prog-descr))) res))
		      (node-programs node-value))
			res))))
      
      ;; 'get-own-programs -> L(program port L(device-type device-name))
      (get-own-programs
       (known-nodes 'get-node-programs *my-node-name*))
      
      ;; 'get-all-programs ->L(node L(program L(device-type device-name))) !!! TODO
      (get-all-programs
       (let ((res nil))
	 (maphash #'(lambda (nd-name nd-descr)
		      (null nd-descr) ;;;; !!! fucking bydlocode to eliminate warning. Remake.
		      (push (list nd-name (known-nodes 'get-node-programs nd-name)) res))
		  kn-nodes)
	 res))
      ;; is-node? <node-name> -> t, if the node exists in table, else nil
      (is-node? 
       (nth-value 1 (gethash (car args) kn-nodes)))
      ;; get-devices-of-type <type> -> L(node-name program-name device-name)
      (get-devices-of-type
       (let ((res nil))
	 (maphash #'(lambda (node-name node-descr)
				(format t "1___~A~&" node-descr)
		      (maphash #'(lambda (prog-name prog-descr)
					(format t "2___~A~&" prog-descr)
				   (mapcar #'(lambda (dev-descr)
					(format t "3___~A~&" dev-descr)
						;;; if type of device is correct, add it to result
					       (when (equal (device-type dev-descr) (car args))
						 (push (list node-name prog-name (device-name dev-descr))
						       res)))
					   (program-devices prog-descr)))
			       (node-programs node-descr)))
		  kn-nodes)
	 res))
      ;; get-device-connection-data node program device -> <is-device?> address port
      (get-device-connection-data
	 (if (and (known-nodes 'is-node? (car args)) ; there is node
		  (let ((node-progs (node-programs (gethash (car args) kn-nodes))))
			(nth-value 1 (gethash (cadr args) node-progs))) ; there is program in node
		  (some #'(lambda (dev-descr) 
			    (equal (device-name dev-descr) (caddr args)))
			(program-devices (gethash (cadr args) (node-programs (gethash (car args) kn-nodes)))))) ; there is device in program
	     (list t 
		   (node-address (gethash (car args) kn-nodes)) 
		   (program-port (gethash (cadr args) (node-programs (gethash (car args) kn-nodes)))))
	     (list nil nil nil)))
      ;; get-own-program-connection-data program-name -> port | nil 
      (get-own-program-connection-data
       (when (known-nodes 'is-my-program? (car args))
	   (program-port (gethash (car args) (node-programs (gethash *my-node-name* kn-nodes))))))
      (t ;;; !!! ADD exception here
       (format t "known-nodes: unknown command ~A~&" cmd)
       nil))))

(trace known-nodes)
(defvar known-nodes-trace t) ; !!! DEBUG
	   
	   
; planned actions list --- add!!!

; send message to another server
(defmacro answer-for-server (server-ip &rest message-data)
  (let ((tempvar (gensym))
	(tval (append (list 'format 'nil) message-data)))
    `(let ((,tempvar (string-to-array ,tval)))
       (usocket:socket-send (usocket:socket-connect 
			     ,server-ip *my-node-port* :protocol :datagram) ,tempvar (length  ,tempvar)))))

; send message to program
(defmacro answer-for-program (port &rest message-data)
  (let ((tempvar (gensym))
	(tval (append (list 'format 'nil) message-data)))
    `(let ((,tempvar (string-to-array ,tval)))
       (usocket:socket-send (usocket:socket-connect 
			     *my-node-address* ,port :protocol :datagram) ,tempvar (length  ,tempvar)))))

; messages listener
(defun socket-listener-function ()   
	(let ((socket-listener (usocket:socket-connect ;;; !!! add error ADDRINUSE processing
				  nil nil :protocol :datagram :local-host *my-node-address* :local-port *my-node-port*)))
				  (loop
  (multiple-value-bind (received-data data-length sender-ip sender-port) (usocket:socket-receive socket-listener nil 200)
    (let ((rec-message (split-data (string-upcase (array-to-string received-data data-length)))))
      (format t "RECIEVED ~A ~A ~A ~A~&" rec-message data-length sender-ip sender-port)
      (cond 
	;; messages from nodes
	;; message "IAM NODE <node-name>" - if we don't know it, answer "WHOIS NODE"
	((compare-lists rec-message '("IAM" "NODE"))
	 (if (not (known-nodes 'add-node (caddr rec-message) sender-ip))
	     (answer-for-server sender-ip "WHOIS NODE ~A" (caddr rec-message))))
	;; message "WHOIS NODE <node-name>" - if name is our, send our info "PROGRAMSOF". 
	;; !!! 1) - no defence , 2) --- redirection !!!
	((compare-lists rec-message '("WHOIS" "NODE"))
		(when (equal (caddr rec-message) *my-node-name*)
			(answer-for-server sender-ip "PROGRAMSOF ~A ~A" *my-node-name* (serialize-data (known-nodes 'get-own-programs)))))
	;; message "PROGRAMSOF <node-name> L(program port L(device-type device-name))"
	((compare-lists rec-message '("PROGRAMSOF"))
	 (known-nodes 'add-programs (cadr rec-message) (caddr rec-message)))
	;; message "RENEWED <node-name>" - answer "WHOIS"
	((compare-lists rec-message '("RENEWED"))
	 (answer-for-server sender-ip "WHOIS NODE ~A" (caddr rec-message)))
; ((string-equal rec-message "please-start 'your "))
	;; messages from clients
	;; message "IAM PROGRAM <program-name> <port>" - if we don't know it, answer "WHOIS PROGRAM"
	((compare-lists rec-message '("IAM" "PROGRAM"))
	 ;;; if program under question not here - answer "WHOIS" !!! add port updating
	 (when (not (known-nodes 'is-my-program? (caddr rec-message)))
		(let ((prog-port (cadddr rec-message)))
		(known-nodes 'add-own-programs (list (list (caddr rec-message) prog-port)))
	  (answer-for-program prog-port "WHOIS PROGRAM ~A" (caddr rec-message)))))
	;; message "DEVICESOF <program-name> L(<device-type> <device-name>)" - add devices
	((compare-lists rec-message '("DEVICESOF"))
	 (known-nodes 'add-devices-of-program  (cadr rec-message) (caddr rec-message)))
	;; message "GIVEME MYNODENAME <program-name>" -> "TAKE YOURNODENAME <node-name>"
	((compare-lists rec-message '("GIVEME" "MYNODENAME"))
		(answer-for-program 
			(known-nodes 'get-own-program-connection-data (caddr rec-message)) 
			"TAKE YOURNODENAME ~A" *my-node-name*))
	;; message "GIVEME DEVICES <sender> <type>" -> "TAKE DEVICES <type> L(node program device-name)"
	((compare-lists rec-message '("GIVEME" "DEVICES"))
	 (if (known-nodes 'is-my-program? (caddr rec-message))
		(answer-for-program (known-nodes 'get-own-program-connection-data (caddr rec-message))
			     "TAKE DEVICES ~A ~A" 
			     (cadddr rec-message)
				 (known-nodes 'get-devices-of-type (cadddr rec-message)))
		(add-log-message "reciever" (format nil "Unknown program ~A sent GIVEME DEVICE to me" (caddr rec-message)))))
	;; message "GIVEME CONNECTION <sender-name> (<node> <prog> <device-name>)"
	((compare-lists rec-message '("GIVEME" "CONNECTION"))
		(let* ((sender (cadddr rec-message))
			(dev-conn (known-nodes 'get-device-connection-data (car sender) (cadr sender) (caddr sender))))
	   (answer-for-program (known-nodes 'get-own-program-connection-data (caddr rec-message))
			       "TAKE CONNECTION (~A ~A ~A) (~A ~A)"  
			       (car sender) (cadr sender) (caddr sender)
			       (cadr dev-conn) (caddr dev-conn))))
	;; ((string-equal rec-message "renewed  "))
;	((compare-lists rec-message '("PLEASE-START" "FUNCTION")) ; !!! not finished yet
;	 (answer-for-program  "PLEASE-START FUNCTION ~A ~A ~A" (caddr rec-message)))
      (t (add-log-message "reciever" (format nil "Unknown command received: ~A" (caddr rec-message)))))
    )))))

; function for network listener managing.
; !!! issues: no connection error processing.
  
(let ((thread-listener nil)) 
  (defun main-socket-listener (cmd)
    (cond ((eq cmd 'close) (sb-thread:terminate-thread thread-listener))
	  ((eq cmd 'start)  
	   (setq thread-listener
		 (sb-thread:make-thread #'socket-listener-function :name "thread-listener")))
	  (t nil))))

; broadcast message formatter
  (defun make-message-for-broadcast (cmd) ; !!! args?
    (string-to-array (cond ((eq cmd 'iam) (format nil "IAM NODE ~A"  *my-node-name*))
			   ((eq cmd 'renewed)  (format nil "RENEWED NODE ~A" *my-node-name*))
			   (t (format nil "~A" '(foo))))))

; broadcast sender
; !!! issues: 1) args ?
;	2) make socket for each address in beginning
  (defun send-broadcast-message-to-servers (cmd) 
    (let ((ms (make-message-for-broadcast cmd)))
	  (dolist (remote-node-address *multicast-addresses*)
		(usocket:socket-send (usocket:socket-connect remote-node-address *my-node-port* :protocol :datagram) ms (length  ms)))))

; periodical automatic sender
(defun periodic-sender-function ()
  (progn (sleep 3)
	 (send-broadcast-message-to-servers 'iam)
	 (periodic-sender-function)))

(let ((thread-automatic-sender nil))
  (defun periodic-sender (cmd) ; !!! &rest args) ?
    (cond ((eq cmd 'close) (sb-thread:terminate-thread thread-automatic-sender))
	  ((eq cmd 'start) (setf thread-automatic-sender (sb-thread:make-thread #'periodic-sender-function))) ;(process-run-function *thread-automatic-sender* #'periodic-sender-function))
	  (t nil))))

