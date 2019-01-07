(asdf:defsystem tiny-lisp-blockchain
  :version "0.1"
  :author "Andy Driver"
  :license "MIT"
  :depends-on (:caveman2
               :cl-json
               :clack
               :dexador
               :envy
               :ironclad
               :local-time
               :uuid
               :zenekindarl)
  :components ((:file "config")
               (:file "blockchain")
               (:file "main"))
  :description "Tiny Lisp Blockchain")
