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
  (:export #:new-server
           ;;servers
           #:get-server
           #:server
           #:start-server
           #:stop-server
           ;;websocket
           #:websocket

           #:server
           #:port
           #:thread
           #:paths
           #:cap
           #:origins
           ;;user-funs
           #:send-pong
           #:send-ping
           #:send
           #:close

           #:on-open
           #:on-message
           #:on-close
           #:on-condition

           ;;helpers
           #:*log*

           ;;conditions
           #:portal-condition
           #:server-condition
           #:no-known-server
           #:server-already-exists-by-key
           #:invalid-status-code
           #:excess-length
           #:length-exceeded
           #:read-failure
           #:rsv-bit-set
           #:missing-headers
           #:upgrade-problem
           #:not-utf8
           #:close-already-sent))
