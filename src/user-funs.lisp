(in-package #:portal)

#||
This file contains code for the functions that are written by the user.. ie they
go in the body of one of the resource methods.
||#

(defun send-control-frame (websocket op message)
  "Send control frame on WEBSOCKET with MESSAGE of OP. Message cant exceed 125 bytes."
  (when (< 125 (length message))
    (error 'excess-length
           :fun #'send-pong
           :max 125))
  (with-slots ((websocket-stream websocket-stream))
      websocket
    (let ((seq (list (+ #b10000000 op)
                     (length message))))
      (write-sequence seq websocket-stream)
      (force-write message websocket-stream))))

(defmethod send-pong ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (logging "Sending PONG with message: ~A~%" message)
  (send-control-frame websocket +pong+ message))
  
(defmethod send-ping ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (logging "Sending PING with message: ~A~%" message)
  (send-control-frame websocket +ping+ message))

(defmethod send ((websocket websocket) message)
  ;;send does not support fragmentation.
  ;;we dont need then as we are just acting as a server.
  ;;if there was a client driver as well then we would 100% need the ability to fragment
  (logging "Sending message of length: ~D.~%To Websocket: ~A.~%"
           (length message) websocket)
  (let* ((stream (socket-stream websocket))
         (binary (typep message '(array (unsigned-byte 8))))
         (message (if (typep message '(or string (array (unsigned-byte 8))))
                      message
                      (format nil "~a" message)))
         (message (if binary
                      message
                      (string-to-octets message)))
         (len (length message)))
    (declare (type number len))
    (write-byte
     (+ #b10000000                      ;fin not fragmented
        (if binary                      ;opcode
            +binary+
            +text+))
     stream)
    (write-length stream len)
    ;; payload
    (force-write message stream)
    message))

(defmethod send-close-frame ((websocket websocket))
  (logging "Sending close frame.~%")
  ;;we are supposed to add a reason and a code...
  (force-write (list (+ #b10000000 +close+) 0)
               (socket-stream websocket)))

(defgeneric close (server websocket)
  (:documentation
   #.(ds "Called by the server creator to close the WEBSOCKET."))
  (:argument-precedence-order websocket server)
  (:method (server (websocket closed))
    nil)
  (:method (server (websocket websocket))
    ;;to close we want to send a close frame.
    ;;and mark as closing.
    ;;
    (logging "Closing.~%")
    (send-close-frame websocket)
    ;; move to closing state
    (change-class websocket 'closing)
    ;;if the other side closes their stream because this is sent then it'll stream-error
    ;;and it'll get marked as closed anyway
    ))
