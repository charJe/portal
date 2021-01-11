(defpackage portal-asd
  (:use #:cl #:asdf))
(in-package #:portal-asd)

(defsystem #:portal
  :description "Portable websockets."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "0"
  :serial t
  :depends-on (#:usocket-server
               #:arrows
               #:global-vars
               #:str
               #:sha1
               #:flexi-streams)
  :components ((:file "package")
               (:file "portal" :depends-on ("package"))))
