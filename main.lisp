(defpackage tiny-lisp-blockchain
  (:use :cl :caveman2)
  (:import-from :cl-json
                :encode-json-to-string)
  (:import-from :tiny-lisp-blockchain.blockchain
                :blockchain
                :blockchain-to-json
                :is-valid
                :mine
                :node
                :transaction)
  (:import-from :zenekindarl
                :render-file))

(in-package tiny-lisp-blockchain)

(defparameter *app* (make-instance '<app>))
(defparameter *node* (make-instance 'node))
(defparameter +templates+ (asdf:system-relative-pathname :tiny-lisp-blockchain #p"templates/"))

(defun template (name &optional (ext "html"))
  (make-pathname :defaults +templates+ :name name :type ext))

(defun render (name &rest params)
    (apply #'render-file (append `(,(template name) :backend :string) params)))

(defun form-field (name form-data)
  (cadr (assoc name form-data :test #'string=)))


(defroute index "/" ()
  (render "home" :name "foobar"))

(defroute ("/transaction" :method :POST) (&key _parsed)
  (let ((from (form-field "from" _parsed))
        (to (form-field "to" _parsed))
        (amount (form-field "amount" _parsed)))
    (transaction *node* `(("from" . ,from)
                          ("to" . ,to)
                          ("amount" . ,amount)))
    (render "transaction" :from from :to to :amount amount)))

(defroute "/blocks" ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (blockchain-to-json *node* ))

(defroute "/mine" ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (mine *node*))