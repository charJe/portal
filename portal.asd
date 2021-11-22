(in-package #:asdf-user)

(defsystem #:portal
  :description "Portable websockets."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "1.2"
  :serial t
  :depends-on
  (#:usocket-server
   #:alexandria
   #:arrows
   #:global-vars
   #:str
   #:ironclad
   #:cl-base64
   #:flexi-streams
   #:parse-float)
  :components
  ((:file "package")
   (:file "portal")))
