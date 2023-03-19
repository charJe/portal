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
    (logging "Handling condition: ~A~%" condition)
    (call-next-method)
    (force-close websocket)
    ;;spec says that we should try to read and disgard any bytes.. but says
    ;;MAY just yeet the connection
    ;; move to closing state
    ))

(defmethod handle-condition :after (condition server websocket)
  (change-class websocket 'closed)
  (on-close (path websocket) server websocket))
    
(defun force-close (websocket)
  (logging "Forcing socket to close.")
  (with-accessors ((socket-stream socket-stream))
      websocket
    (handler-case (cl:close socket-stream)
        ;;if stream-error is signalled then we know that the client shut the connection     
      (stream-error ()
        t))))
                    
(defmethod handle-condition ((c portal-condition) server websocket)
  ;;here we are just going to kill the connection.
  (logging "Using fallback condition handler. Killing stream.~%")
  (send-close-frame websocket :code :FATAL 
                              :reason (format nil "Fatal error. ~A" (type-of c))))

(defmethod handle-condition ((c bad-mask) server websocket)  
  (send-close-frame websocket :code :protocol-error
                              :reason (format nil "Bad mask.")))

(defmethod handle-condition ((c read-failure) server websocket)  
  (send-close-frame websocket :code :FATAL
                              :reason (format nil "Cannot read anymore.")))

(defmethod handle-condition ((c length-exceeded) server websocket)
  "When a payload exceeds its maximum capacity."
  (send-close-frame websocket :code :OVERSIZE 
                              :reason (format nil "Length Exceeded in ~A."
                                              (type-of (frame c)))))

(defmethod handle-condition ((c excess-length) server websocket)
  "This is a problem only when there are programming errors."
  (send-close-frame websocket :code :FATAL 
                              :reason (format nil "Excess len in: ~A" (fun c)))



  

