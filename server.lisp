(require :asdf)
;(ql:quickload 'usocket)
(asdf:operate 'asdf:load-op :usocket)
;(asdf:oos 'asdf:load-op :usocket)
;(use-package 'usocket)

; UTILITIES
(load #p"./utils.lisp")

(load #p"./server_internal.lisp")

; INITIAL DATA
(load #p"./local_config.lisp")

;initial values for internal variables
(known-nodes 'add-node *my-node-name* *my-node-address*)
(known-nodes 'add-programs *my-node-name* (list (list "local_server" *my-node-port* '(("NODE-SERVER" "server0")))))

; start server

(main-socket-listener 'start)
(periodic-sender 'start)


