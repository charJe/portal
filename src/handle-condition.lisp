(in-package #:portal)

#||
This file contains code for handling unexpected conditions.
This could be a failure to conform with the standard or an excess length, or perhaps a
stream was shut when it shouldn't have been.
||#

(defgeneric handle-condition (condition server websocket)
  (:documentation
   #.(ds "Attempt to handle condition CONDITION correctly."))
  (:method :around (condition server websocket)
    (logging "Handling condition: ~A~%" condition)))

(defun force-close (websocket)
  (logging "Forcing socket to close.")
  (with-accessors ((socket-stream socket-stream))
      websocket
    (handler-case (progn (cl:close socket-stream :abort t)
                         t)
        ;;if stream-error is signalled then we know that the client shut the connection     
      (stream-error ()
        t))))
                    
(defmethod handle-condition ((c portal-condition) server websocket)
  ;;here we are just going to kill the connection.
  (logging "Using fallback condition handler. Killing stream.~%")
  (with-accessors ((socket-stream socket-stream)
                   (path path))
      websocket
    (force-close websocket)
    ;;spec says that we should try to read and disgard any bytes.. but says
    ;;MAY just yeet the connection
    ;; move to closing state
    (change-class websocket 'closed)
    ;; run function
    (on-close path server websocket)))

(defmethod handle-condition ((c excess-length) server websocket)
  (send-close-frame websocket);;missing code and reason
  (force-close websocket)
  (change-class websocket 'closed)
  (on-close (path websocket) server websocket))
  

