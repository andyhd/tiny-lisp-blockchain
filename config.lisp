(defpackage tiny-lisp-blockchain.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export config))
(in-package tiny-lisp-blockchain.config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  '(peers ()))

(defconfig |node1|
  '(peers ("node2:5002"
           "node3:5003")))

(defconfig |node2|
  '(peers ("node1:5001"
           "node3:5003")))

(defconfig |node3|
  '(peers ("node1:5001"
           "node2:5002")))

(defun config (&optional key)
  (let ((value (envy:config #.(package-name *package*) key)))
    (format t "config~%  key: ~A~%  value: ~A~%" key value)
    value))
