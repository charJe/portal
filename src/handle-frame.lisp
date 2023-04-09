(in-package #:portal)

#||
This file contains the code for handling each frame.

||#


(defgeneric handle-frame (server frame websocket)
  (:documentation "Attempt to correctly handle FRAME for WEBSOCKET.")
  (:argument-precedence-order frame websocket server)
  (:method (server (frame frame) (websocket websocket))
    (error 'unsupported-frame :frame frame))
  (:method (server (frame frame) (websocket closing))
    (error 'close-already-sent))            
  (:method :around (server (frame frame) (websocket websocket))
    (logging "Handling frame: ~A~%For Websocket: ~A~%"
             frame websocket)
    (call-next-method)))

(defmethod handle-frame :before (server (frame has-payload) (websocket ready))
  (with-accessors ((len len)
                   (mask mask)
                   (payload payload))
      frame
    (logging "Reading a payload~%")
    (unless (zerop len)
      (setf payload (read-payload server frame len mask (socket-stream websocket))))))
            
(defmethod handle-frame (server (frame ping) (websocket ready))
  (with-accessors ((payload payload))

      frame
    (send-pong websocket (or payload (make-array 0 :element-type '(unsigned-byte 8))))))
     
(defmethod handle-frame (server (frame pong) (websocket ready))
  (with-accessors ((payload payload))
      frame
    (logging "PONG received: ~A~%" (octets-to-string payload))
    payload));;we dont actually have to do anything here.
          
(defmethod handle-frame (server (frame continuation) (websocket closing))
  (logging "Websocket received continuation frame after sending closing frame... ~%")
  (logging "... continuing to read ~%")
  (call-next-method))
                                
(defmethod handle-frame (server (frame continuation) (websocket ready))
  (logging "Continuation frame.~%")
  (logging "Changing class of continuation to ~A~%." (continuation-type websocket))
  (change-class frame (continuation-type websocket))
  (handle-frame server frame websocket))

(defmethod handle-frame (server (frame text) (websocket ready))
  (with-accessors ((payload payload))
      frame
    (let ((text (handler-case
                    ;; payload must be valid utf-8
                    (octets-to-string payload)
                  (serious-condition ()
                    (error 'not-utf8)))))
      (logging "Text received: ~A.~%" text)
      (on-message (path websocket) server websocket text))))

(defmethod handle-frame (server (frame binary) (websocket ready))
  (with-accessors ((payload payload))
      frame
    (logging "Binary received: ~A.~%" payload)
    (on-message (path websocket) server websocket payload)))

(defun extract-code-and-reason (frame)
  "Extract the code and the reason from close frame."
  (with-accessors ((payload payload)
                   (code code)
                   (reason reason))
      frame
    (when payload
      (logging "Close frame has code and reason.~%")
      (let ((code-ar (subseq payload 0 2))
            (reason-ar (subseq payload 2)))
        (setf reason
              (handler-case
                  ;; payload must be valid utf-8
                  (octets-to-string reason-ar)
                (serious-condition ()
                  (error 'not-utf8)))
              code (nibbles:ub16ref/be code-ar 0))))))
                        
(defmethod handle-frame (server (frame close) (websocket ready))
  (extract-code-and-reason frame)
  (with-accessors ((code code)
                   (reason reason))
      frame
    (when (and code reason)
      (logging "Code: ~D.~%Reason: ~A.~%" code reason))
    (send-close-frame websocket :code (or code :NORMAL))
    (force-close websocket)
    (change-class websocket 'closed)
    (on-close (path websocket) server websocket)))
