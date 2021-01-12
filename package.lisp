(defpackage portal
  (:nicknames pws)
  (:use #:cl)
  (:shadow close)
  (:import-from #:usocket
                socket
                socket-server
                *wildcard-host*)
  (:import-from #:arrows
                -> ->> -<>>)
  (:import-from #:global-vars
                define-global-var
                define-global-parameter)
  (:export
   define-resource
   websocket
   send
   send-ping
   close
   server
   server-close
   *debug-on-error*))
