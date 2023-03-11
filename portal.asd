(in-package #:asdf-user)

(defsystem #:portal
  :description "Portable websockets."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "1.2"
  :serial t
  :depends-on (#:usocket-server
               #:alexandria
               #:arrows
               #:str
               #:fast-http
               #:cl-base64
               #:ironclad
               #:babel
               #:nibbles
               #:parse-float)
  :pathname "src"
  :components ((:file "package")
               (:file "helpers")
               (:file "conditions")
               (:file "classes")
               (:file "resource")
               (:file "http")
               (:file "portal")
               (:file "servers")))

