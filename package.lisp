(defpackage portal
  (:nicknames :pws)
  (:use #:cl #:usocket #:arrows)
  (:shadow :close)
  (:export :define-path-handler
           :websocket
           :send
           :send-ping
           :close
           :websocket-server
           :websocket-server-close
           :*debug-on-error*))
