(in-package #:portal)

#||
This file contains code for the functions that are written by the user.. ie they
go in the body of one of the resource methods.
||#

(defun send-control-frame (websocket op &key message code)
  "Send control frame on WEBSOCKET with MESSAGE of OP. Message cant exceed 125 bytes."
  (check-type op (unsigned-byte 8))
  (check-type message (or null (array (unsigned-byte 8) (*))))
  (check-type code (or null (unsigned-byte 8)))
  (with-accessors ((socket-stream socket-stream))
      websocket
    (when (and message (< 125 (length message)))
      (error 'excess-length
             :fun #'send-control-frame
             :max 125))
    (let ((seq (list (+ #b10000000 op)
                     (length message))))
      (write-sequence seq socket-stream)
      (when code
        (nibbles:write-ub16/be code socket-stream))
      (force-write message socket-stream))))

(defgeneric send-pong (websocket message)
  (:method ((websocket websocket) (message array))
    (logging "Sending PONG with message as BINARY: ~A~%" (octets-to-string message))
    (send-control-frame websocket +pong+ :message message))
  (:method ((websocket websocket) (message string))
    (send-pong websocket (string-to-octets message))))
  
(defgeneric send-ping (websocket message)
  (:method ((websocket websocket) (message array))
    (logging "Sending PING with message as BINARY: ~A~%" (octets-to-string message))
    (send-control-frame websocket +ping+ :message message))
  (:method ((websocket websocket) (message string))
    (send-ping websocket (string-to-octets message))))

(defmethod send ((websocket websocket) (message string))
  ;;send does not support fragmentation.
  ;;we dont need then as we are just acting as a server.
  ;;if there was a client driver as well then we would 100% need the ability to fragment
  (logging "Sending STRING message of length: ~D.~%To Websocket: ~A.~%"
           (length message) websocket)
  (let* ((stream (socket-stream websocket))
         (len (length message)))
    (declare (type number len))
    (write-byte (+ #b10000000 +text+) stream)
    (write-length stream len)
    ;; payload
    (force-write (string-to-octets message) stream)
    message))

(defmethod send ((websocket websocket) (message array))
    (logging "Sending BINARY message of length: ~D.~%To Websocket: ~A.~%"
           (length message) websocket)
  (let* ((stream (socket-stream websocket))
         (len (length message)))
    (declare (type number len))
    (write-byte (+ #b10000000 +binary+) stream)
    (write-length stream len)
    ;; payload
    (force-write message stream)
    message))
 
(defun send-close-frame (websocket &key code reason)
  (logging "Sending close frame.~%")
  ;;we are supposed to add a reason and a code...
  (when code (check-valid-status-code code))
  (send-control-frame websocket +close+
                      :code (when code (code-value code))
                      :message reason))

(defgeneric close (server websocket &key code reason)
  (:documentation
   #.(ds "Called by the server creator to close the WEBSOCKET."))
  (:argument-precedence-order websocket server)
  (:method :before (server websocket &key code reason)
    (declare (ignore reason))
    (when code (check-valid-status-code code)))
  (:method (server (websocket closed) &key code reason)
    (declare (ignore code reason))
    nil)
  (:method (server (websocket websocket) &key code reason)
    ;;to close we want to send a close frame.
    ;;and mark as closing.
    ;;
    (logging "Closing.~%")
    (send-close-frame websocket
                      :code (code-value code)
                      :reason (when reason 
                                (string-to-octets reason)))
    ;; move to closing state
    (change-class websocket 'closing)
    ;;if the other side closes their stream because this is sent then it'll stream-error
    ;;and it'll get marked as closed anyway
    ))
