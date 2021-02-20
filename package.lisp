(defpackage portal
  (:nicknames pws)
  (:use #:cl)
  (:shadow close)
  (:import-from #:alexandria
                define-constant)
  (:import-from #:usocket
                socket
                socket-server
                *wildcard-host*)
  (:import-from #:arrows
                -> ->> -<>>)
  (:import-from #:global-vars
                define-global-var
                define-global-parameter)
  (:import-from #:read-float
                read-float)
  (:export
   define-resource
   websocket
   header
   ready-state
   send
   send-ping
   close
   server
   server-close
   *debug-on-error*))
