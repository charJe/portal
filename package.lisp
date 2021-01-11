(defpackage portal
  (:nicknames :pws)
  (:use #:cl)
  (:shadow :close)
  (:import-from #:usocket
   :socket
   :socket-server
   :*wildcard-host*)
  (:import-from #:arrows
   :-> :->> :-<>>)
  (:export
   :define-path-handler
   :websocket
   :send
   :send-ping
   :close
   :server
   :server-close
   :*debug-on-error*))
