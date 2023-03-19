(defpackage #:portal
  (:nicknames #:pws)
  (:use #:cl)
  (:shadow #:close)
  (:import-from #:alexandria
                #:define-constant)
  (:import-from #:usocket
                #:socket
                #:socket-server
                #:*wildcard-host*)
  (:export #:websocket
           #:header
           #:ready-state
           #:send
           #:send-ping
           #:close
           #:server
           #:server-close))
