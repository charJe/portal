(in-package #:asdf-user)

(defsystem #:portal
  :description "Portable websockets."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "2.0"
  :serial t
  :depends-on (#:usocket-server
               #:alexandria
               #:str
               #:local-time
               #:fast-http
               #:cl-base64
               #:ironclad
               #:babel
               #:nibbles)
  :pathname "src"
  :components ((:file "package")
               (:file "helpers")
               (:file "conditions")
               (:file "classes")
               (:file "resource")
               (:file "http")
               (:file "handle-condition")
               (:file "handle-frame")
               (:file "portal")
               (:file "user-funs")
               (:file "servers")))

