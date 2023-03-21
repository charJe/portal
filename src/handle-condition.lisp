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
    (call-next-method)
    (force-close websocket)
    ;;spec says that we should try to read and disgard any bytes.. but says
    ;;MAY just yeet the connection
    ;; move to closing state
    )
  (:method (condition server websocket)
    (logging "Resignalling condition in #'handle-condition. ~%")
    (error condition)))

(defmethod handle-condition :after (condition server websocket)
  (change-class websocket 'closed)
  (on-close (path websocket) server websocket))
    
(defun force-close (websocket)
  (logging "Forcing socket to close.")
  (with-accessors ((socket-stream socket-stream))
      websocket
    (let ((open? (open-stream-p socket-stream)))
      (if open? 
          (handler-case 
              (cl:close socket-stream)
            ;;if stream-error is signalled then we
            ;;know that the client shut the connection     
            (stream-error ()
              t))
          (progn (logging "Stream already closed.~%")
                 t)))))

                
(defmethod handle-candition ((c upgrade-problem) server websocket)
  (let ((reason (string-downcase (code c))))
    (logging "Cannot upgrade: ~A~%" reason)
    (force-write (build-header +http-version+ 400 "Bad Request"                            
                               :reason reason)
                 (socket-stream websocket))))

                    
(defmethod handle-condition ((c portal-condition) server websocket)
  ;;here we are just going to kill the connection.
  (logging "Using fallback condition handler. Killing stream.~%")
  (send-close-frame websocket :code :FATAL 
                              :reason (format nil "Fatal error. ~A" (type-of c))))

(defmethod handle-condition ((c not-utf8) server websocket)
  ;;here we are just going to kill the connection.
  (send-close-frame websocket :code :INCOSISTENT-TYPE
                              :reason "Text should be UTF8."))

(defmethod handle-condition ((c mask-not-set) server websocket)  
  (send-close-frame websocket :code :protocol-error
                              :reason "Mask bit not set."))

(defmethod handle-condition ((c rsv-bit-set) server websocket)  
  (send-close-frame websocket :code :protocol-error
                              :reason "RSV bit set. Unsupported."))

(defmethod handle-condition ((c read-failure) server websocket)
  (logging "Stream is open? ~A~%" (open-stream-p (socket-stream websocket)))
  (when (open-stream-p (socket-stream websocket))
    (send-close-frame websocket :code :FATAL
                                :reason "Cannot read anymore.")))

(defmethod handle-condition ((c length-exceeded) server websocket)
  "When a payload exceeds its maximum capacity."
  (send-close-frame websocket :code :OVERSIZE 
                              :reason (format nil "Length Exceeded in ~A."
                                              (type-of (frame c)))))

(defmethod handle-condition ((c excess-length) server websocket)
  "This is a problem only when there are programming errors."
  (send-close-frame websocket :code :FATAL 
                              :reason (format nil "Excess len in: ~A" (fun c))))



(defgeneric fallback-condition-handler (condition server websocket)
  (:documentation
   #.(ds "Absolute last resort means of handling a condition. ~
          This is used when a condition is signalled that we do not expect at all."))
  (:method (condition server websocket)
    (logging "Fatal condition in #'websocket-handler: ~A~%" condition)
    (force-close websocket)
    nil))