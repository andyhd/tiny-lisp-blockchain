(ql:quickload :tiny-lisp-blockchain)

(in-package tiny-lisp-blockchain)

(defvar *port* (parse-integer (or (asdf::getenv "PORT") "5000")))

(defparameter *server* (clack:clackup *app*
                                      :server :woo
                                      :address "0.0.0.0"
                                      :port *port*
                                      :use-thread nil))

; (clack:stop *server*)
