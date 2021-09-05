(defpackage portal-asd
  (:use #:cl #:asdf))
(in-package #:portal-asd)

(defsystem #:portal
  :description "Portable websockets."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "1.1"
  :serial t
  :depends-on (#:usocket-server
               #:alexandria
               #:arrows
               #:global-vars
               #:str
               #:ironclad
               #:cl-base64
               #:flexi-streams
               #:parse-float)
  :components ((:file "package")
               (:file "portal" :depends-on ("package"))))
