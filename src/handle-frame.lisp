(in-package #:portal)

#||
This file contains the code for handling each frame.

||#


(defgeneric handle-frame (server frame websocket)
  (:documentation "Attempt to correctly handle FRAME for WEBSOCKET.")
  (:argument-precedence-order frame websocket server)
  (:method (server frame websocket)
    (error 'unsupported-frame :frame frame))
  (:method :around (server (frame frame) websocket)
    (logging "Handling frame: ~A~%For Websocket: ~A~%"
             frame websocket)
    (call-next-method)))

(defmethod handle-frame (server (frame ping) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (send-pong websocket (if (zerop len)
                             (make-array 0 :element-type '(unsigned-byte 8))
                             (read-payload server frame len mask (socket-stream websocket))))))

(defmethod handle-frame (server (frame pong) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (unless (zerop len)
      ;;no payload if its 0
      (read-payload server frame len mask (socket-stream websocket)))))
      
(defmethod handle-frame (server (frame continuation) websocket)
  (logging "Continuation frame.~%")
  (logging "Changing class of continuation to ~A~%." (continuation-type websocket))
  (change-class frame (continuation-type websocket))
  (handle-frame server frame websocket)
  (on-fragmentation (path websocket) server websocket (stash websocket)))

(defmethod handle-frame (server (frame text) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (let* ((payload (read-payload server frame len mask (socket-stream websocket)))
           (text (handler-case
                     ;; payload must be valid utf-8
                     (octets-to-string payload)
                   (serious-condition ()
                     (error 'not-utf8)))))
      (logging "Text received: ~A.~%" text)
      (on-message (path websocket) server websocket text))))

(defmethod handle-frame (server (frame binary) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (let* ((binary (read-payload server frame len mask (socket-stream websocket))))
      (logging "Binary received: ~A.~%" binary)
      (on-message (path websocket) server websocket binary))))

(defmethod handle-frame (server (frame close) (websocket closing))
  (send-close-frame websocket);;missing code and reason
  (force-close websocket)
  (change-class websocket 'closed)
  (on-close (path websocket) server websocket))
