(in-package #:portal)

(define-constant +sec-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=)

(define-constant +http-version+
  "HTTP/1.1"
  :test #'string=)


(defun check-can-upgrade (server http-request)
  #.(ds "True if the HTTP-REQUEST has qualified to be upgraded to a websocket. ~
         Signals 'upgrade-problem if it cannot.")
  (with-accessors ((method fast-http:http-method)
                   (major-version fast-http:http-major-version)
                   (minor-version fast-http:http-minor-version)
                   (resource resource)
                   (origin origin)
                   (upgrade upgrade)
                   (connection connection))
      http-request
    (with-accessors ((paths paths)
                     (origins origins))
        server 
      (logging "Method: ~A~%Major Version: ~A~%Minor Version: ~A~%Resource: ~A~% ~
              upgrade: ~A~%Connection: ~A~%"                                          
               method major-version minor-version resource upgrade connection)
      (flet ((c (k) (error 'upgrade-problem :key k)))
        (handler-case
            (and
             ;; method = get
             (or (eq :get method)
                 (c :method))
             ;; http version greater than 1
             (or (and (<= 1 major-version)
                      (< 0 minor-version))
                 (c :version))
             ;; missing sec-websocket-key
             (or (sec-websocket-key http-request)
                 (c :sec-websocket-key))
             ;; cors
             (if origins
                 (or (some (lambda (orig)
                             (string= orig origin))
                           origins)
                     (c :origin))
                 t)
             ;; trying to upgrade
             (or (string= "websocket" upgrade)
                 (c :upgrade))
             (or (str:containsp "Upgrade" connection)
                 (c :connection))
             ;; script
             (or (find resource paths)
                 (c :script)))
        (serious-condition ()
          (c :request)))))))


(defgeneric data-final-type (data)
  (:method ((data text-data))
    'string)
  (:method ((data binary-data))
    'vector))

(defmethod get-stash ((websocket websocket))  
  (with-accessors ((stash stash))
      websocket
    (with-accessors ((stash stash))
        stash 
      (apply #'concatenate (data-final-type (first stash))
             (let ((data ()))
               (dolist (s stash data)
                 (push (data s) data)))))))
                     
(defgeneric reset-stash (stash)
  (:documentation "Reset stash back to its original state.")
  (:method ((stash stash))
    (setf (stash stash) ()))
  (:method ((stash capped-stash))
    (setf (len stash) 0)
    (call-next-method)))

(defmethod clear-stash ((websocket websocket))
  (with-accessors ((stash stash))
      websocket
    (reset-stash stash)))

(defmethod send-pong ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (logging "Sending PONG with message: ~A~%" message)
  (when (< 125 (length message))
    (error 'excess-length
           :fun #'send-pong
           :max 125))
  (with-slots ((websocket-stream websocket-stream))
      websocket
    (let ((seq (list (+ #b10000000 +pong+)
                     (length message))))
      (write-sequence seq websocket-stream)
      (force-write message websocket-stream))))

(defmethod send-ping ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (logging "Sending PING with message: ~A~%" message)
  (when (< 125 (length message))
    (error 'excess-length
           :fun #'send-ping
           :max 125))
  (with-slots ((websocket-stream websocket-stream))
      websocket
    (let ((seq (list (+ #b10000000 +ping+)
                     (length message))))
      (write-sequence seq websocket-stream)
      (force-write message websocket-stream))))

(defun read-length (length stream)
  (logging "Reading length...~%")
  (cond ((<= length 125)
         length)
        ((= length 126)
         (nibbles:read-ub16/be stream))
        ((= length 127)
         (nibbles:read-ub64/be stream))))

(defun read-mask (stream)
  (logging "Reading mask...~%")
  (let ((mask (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (index 4 mask)
      (setf (aref mask index) (eread-byte stream :read-mask)))))
     
(defgeneric read-payload (server frame length mask stream)
  (:documentation
   #.(ds "Attempts to read a payload from STREAM using MASK to unmask. ~
          There are constraints on the size of certain bits of data. ~
          So FRAME is used to check those constraints.")))

(defmethod read-payload :before (server (frame control-frame) length mask stream)
  (when (< +control-frame-max-payload-size+ length)
    (error 'length-exceeded :length length
                            :frame frame)))

(defmethod read-payload :before (server (frame data-frame) length mask stream)
  (let* ((soc (websocket server))
         (stash (stash soc)))
    (when (capped-stash-p stash)
      (with-accessors ((stash stash)
                       (cap cap))
          stash 
        (let ((len (flexi-streams:output-stream-sequence-length stash)))
          (when (< cap (+ len length))
            (error 'length-exceeded
                   :length length
                   :frame frame)))))))
                               
(defmethod read-payload (server frame length mask stream)
  (logging "Reading payload of length ~D.~%" length)
  (let ((payload (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length payload)
      (setf (aref payload index)
            (logxor (eread-byte stream :read-payload)
                    (elt mask (mod index 4)))))))

(defgeneric handle-frame (server frame websocket)
  (:documentation "Attempt to correctly handle FRAME for WEBSOCKET.")
  (:argument-precedence-order frame websocket server)
  (:method (server frame websocket)
    (error 'unsupported-frame :frame frame))
  (:method :around (server (frame frame) websocket)
    (logging "Handling frame: ~A~%For Websocket: ~A~%"
             frame websocket)
    (call-next-method)))

(defgeneric close-frame-p (frame)
  (:method ((c close))
    t)
  (:method (c)
    nil))

(defmethod handle-frame (server (frame ping) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (send-pong websocket (if (zerop len)
                             (make-array 0 :element-type '(unsigned-byte 8))
                             (read-payload len mask (socket-stream websocket))))))

(defmethod handle-frame (server (frame pong) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (unless (zerop len)
      ;;no payload if its 0
      (read-payload len mask (socket-stream websocket)))))
      
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
             
(defun fragmentedp (websocket)
  (continuation-type websocket))

(defun configure-fragmentation (websocket frame)
  (setf (continuation-type websocket) (class-of frame)))

(defun start-of-fragmentation-p (websocket frame fin)
  (when (and (not (fragmentedp websocket));already fragmented
             (not fin);end of frame meaning its only 1 data frame.
             (data-frame-p frame));only text or binary can be fragmented. 
    t))
             
(defmethod read-frame ((websocket websocket))
  #.(ds "Read a from from the stream of WEBSOCKET. ~
         When the frame is the last one in a series, return the complete message. ~
         Could also return :eof, :close, :error.")
  (logging "Reading frame.~%")
  (let* ((stream (socket-stream websocket))
         (b0 (eread-byte stream :eof))
         (b1 (eread-byte stream :eof))                
         (fin (< 0 (logand b0 #b10000000)))
         (opcode (logand b0 #b1111))
         (maskbit (or (< 0 (logand b1 #b10000000))
                   (error 'bad-mask)))
         (len (logand b1 #b1111111))
         (frame (make-frame opcode)))
    (setf (opcode websocket) opcode)
    (when (start-of-fragmentation-p websocket frame fin)          
      ;;if we are not already fragmented and its not finished then we know
      ;;that we are at the 
      (configure-fragmentation websocket frame))
    (logging "b0: ~D. b1: ~D. Fin: ~D. Opcode: ~D. Maskbit: ~D.~%Len: ~D.~%Frame type: ~A.~%"
             b0 b1 fin opcode maskbit len (class-of frame))
      ;; read length
    (let* ((len (read-length len stream))
           ;;read mask
           (mask (read-mask stream)))        
      (setf (len frame) len
            (mask frame) mask)
      (logging "Complete frame read: ~S.~%" frame)
      (values frame fin))))

(defun write-length (stream length)
  (logging "Writing length: ~D." length)
  (let ((val (+ #b00000000                      ;mask
                (cond                           ;length
                  ((<= length 125) length)
                  ((<= length (expt 2 16)) 126)
                  ((< (expt 2 16) length) 127)))))
    (write-byte val stream)
    ;; extended length length
    (cond
      ;; extended twice
      ((< (expt 2 16) length)
       (loop :for place :from 7 downto 0
             :for shift = (expt 2 (* 8 place))
             :do (write-byte
                  (/ (logand length (* #b11111111 shift))
                     shift)
                  stream)))
      ;; extended once
       ((<= 125 length)
       (loop :for place :from 1 downto 0
             :for shift = (expt 2 (* 8 place))
             :do (write-byte
                  (/ (logand length (* #b11111111 shift))
                     shift)
                  stream))))))

(defmethod send ((websocket websocket) message)
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
     (+ #b10000000                      ;fin
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
  (force-write (list (+ #b10000000 +close+) 0)
               (socket-stream websocket)))

(defgeneric close (server websocket)
  (:documentation "Close the WEBSOCKET.")
  (:argument-precedence-order websocket server)
  (:method (server (websocket closed))
    nil)
  (:method (server (websocket websocket))
    (logging "Closing.~%")
    ;; send close frame
    (send-close-frame websocket)
    ;; move to closing state
    (change-class websocket 'closing)
    ;; run function
    (on-close (path websocket) server websocket)))

(defun construct-http-response (&rest list)
  (string-to-octets (alist->header list)))

(defgeneric ready-or-closing-p (websocket)
  (:method ((r ready))
    t)
  (:method ((c closing))
    t)
  (:method (n)
    nil))

(defmethod handle-frame (server (frame portal-condition) websocket)
  (handler-case (cl:close (socket-stream websocket) :abort t)
    (stream-error (c)
      (change-class websocket 'closing)
      (on-close (path websocket) server websocket)
      (error c)))
  ;; close socket
  ;;spec says that we should try to read and disgard any bytes.. but says
  ;;MAY just yeet the connection
  ;; move to closing state
  (change-class websocket 'closing)
  ;; run function
  (on-close (path websocket) server websocket))


(defmethod handle-frame (server (frame close) (websocket closing))
  (close server websocket)
  (cl:close (socket-stream websocket) :abort t))
  
(defun handle-upgrade (server)
  (with-accessors ((websocket websocket))
      server 
    (with-accessors ((socket-stream socket-stream)
                     (header header))
        websocket
      ;; send accept response header
      (force-write (construct-http-response      
                    (cons :version +http-version+)
                    (cons :code 101)
                    (cons :code-meaning "Switching Protocols")
                    (cons :upgrade "websocket")
                    (cons :connection "Upgrade")
                    (cons :sec-websocket-accept
                          (sha1-base64 (str:concat (sec-websocket-key header)
                                                   +sec-key+))))
                   socket-stream)
      ;; custom connect function
      (on-open (path websocket) server websocket)
      (change-class websocket 'ready)
      ;; messages
      (handler-case 
          (read-frame-loop server)
        (stream-error (c)
          (logging "Stream error: ~A" c)          
          nil))
      (change-class websocket 'closed))))
;; move to closed state

(defun read-frame-loop (server)
  (with-accessors ((websocket websocket))
      server 
    (while ((ready-or-closing-p websocket))
      ;; read new payload
      (logging "Websocket: ~A~%" websocket)
      (multiple-value-bind (frame fin)
          (handler-case 
              (read-frame websocket)
            (portal-condition (c)
              (on-condition (path websocket) server websocket c)
              c))
        ;;when its fin we want to call the message resource with the stash        
        (handle-frame server frame websocket)
        (when fin
          (logging "Finished sequence.~%")
          (let ((stashed (get-stash websocket)))
            (clear-stash websocket)
            (on-message (path websocket) server websocket stashed)))))))

            
(defun handle-cannot-upgrade (stream reason)
  (logging "Cannot upgrade: ~A~%" reason)
  (force-write (construct-http-response
                (cons :version +http-version+)
                (cons :code 400)
                (cons :code-meaning "Bad Request")
                (cons :reason reason))
               stream))

(defun websocket-handler (server websocket)
  ;; connect
  (with-accessors ((socket-stream socket-stream)
                   (header header))
      websocket       
    (let* ((request (read-headers socket-stream)))
      (setf header request)
      (handler-case (progn (check-can-upgrade server request)
                           (setf (path websocket)
                                 (pathname (resource (header websocket))))
                           (handle-upgrade server))
        (upgrade-problem (c)
          ;; headers that can't upgrade
          (handle-cannot-upgrade socket-stream (string-downcase (key c))))))))
