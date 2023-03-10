(in-package #:portal)

(defvar *origin* nil
  "If Cross-Origin-Request-Sharing is disallowed, set to the origin (www.example.com).")

(defvar *debug-on-error* nil)

(define-constant +sec-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=)

(define-constant +http-version+
  "HTTP/1.1"
  :test #'string=)

;; frame control codes
(defconstant +continuation+ #x0)
(defconstant +text+ #x1)
(defconstant +binary+ #x2)
(defconstant +close+ #x8)
(defconstant +ping+ #x9)
(defconstant +pong+ #xA)



(defvar *resource-handlers* ()
  "Alist of resource handler functions. Key: path, Value: list of handler functions")

(defun define-resource
    (path &key
            (open (lambda (websocket)
                    (declare (ignore websocket))))
            (message (lambda (websocket message)
                       (declare (ignore websocket message))))
            (close (lambda (websocket)
                     (declare (ignore websocket))))
            (error (lambda (websocket condition)
                     (declare (ignore websocket condition)))))
  (let ((funlist (vector open message close error))
        (resource (assoc (string-downcase path) *resource-handlers*
                         :test #'string=)))
    (if resource
        (setf (cdr resource) funlist)
        (setf *resource-handlers*
              (acons (string-downcase path)
                     funlist
                     *resource-handlers*)))))

(defun get-resource (resource)
  "Get a resource from *resource-handlers*. Signals 'no-defined-resource if one isn't found."
  (or (cdr (assoc resource *resource-handlers* :test #'str:starts-with-p))
      (error 'no-defined-resource
             :resource resource)))

(defun call-resource-function (function-name websocket &rest arguments)
  (restart-case
      (let* ((functions (get-resource (fast-http:http-resource  (header websocket))))
             (function (elt functions
                            (case function-name
                              (:open 0)
                              (:message 1)
                              (:close 2)
                              (otherwise 3)))))
        (handler-bind
            ((error
               (lambda (condition)
                 (funcall (elt functions 3) websocket condition)
                 (unless *debug-on-error*
                   (continue condition)))))
          ;; custom open function
          (apply function websocket arguments)))
    (drop-client ()
      :report "close the client websocket"
      (close websocket))
    (continue ()
      :report "stop processing and continue with normal operations"
      (continue))
    (retry ()
      :report "retry processing"
      (apply #'call-resource-function function-name websocket arguments))))

(defun check-can-upgrade (http-request)
  #.(ds "True if the alist HEADER has qualified to be upgraded to a websocket. ~
         If nil, the second value will be the reason.")
  (with-accessors ((method fast-http:http-method)
                   (major-version fast-http:http-major-version)
                   (minor-version fast-http:http-minor-version)
                   (resource fast-http:http-resource)
                   (origin origin)
                   (upgrade upgrade)
                   (connection connection))
      http-request
    (logging "Method: ~A~%Major Version: ~A~%Minor Version: ~A~%Resource: ~A ~
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
           (if *origin*
               (progn (logging "Origin: ~A. *origin*: ~A" origin *origin*)
                      (or (string= *origin* origin)
                          (c :origin)))
               t)
           ;; trying to upgrade
           (or (string= "websocket" upgrade)
               (c :upgrade))
           (or (str:containsp "Upgrade" connection)
               (c :connection))
           ;; script
           (or (get-resource resource)
               (c :script)))
        (serious-condition ()
          (c :request))))))

(defmethod append-stash ((websocket websocket) (data data))
  (with-accessors ((stash stash))
      websocket
    (logging "Adding ~A of length ~D to stash."
             (class-of data)
             (length (data data)))
    (push data stash)))

(defgeneric data-final-type (data)
  (:method ((data text-data))
    'string)
  (:method ((data binary-data))
    'vector))

(defmethod get-stash ((websocket websocket))
  (with-accessors ((stash stash))
      websocket
    (apply #'concatenate (data-final-type (first stash))
           (let ((data ()))
             (dolist (s stash data)
               (push (data s) data))))))
                     
(defmethod clear-stash ((websocket websocket))
  (with-accessors ((stash stash))
      websocket
    (setf stash ())))

(defmethod send-pong ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (logging "Sending PONG with message: ~A" message)
  (when (< 125 (length message))
    (error "Message cannot be greater than 125 bytes"))
  (with-slots ((websocket-stream websocket-stream))
      websocket
    (let ((seq (list (+ #b10000000 +pong+)
                     (length message))))
      (write-sequence seq websocket-stream)
      (force-write message websocket-stream))))

(defmethod send-ping ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (logging "Sending PING with message: ~A" message)
  (when (< 125 (length message))
    (error "Message cannot be greater than 125 bytes"))
  (with-slots ((websocket-stream websocket-stream))
      websocket
    (let ((seq (list (+ #b10000000 +ping+)
                     (length message))))
      (write-sequence seq websocket-stream)
      (force-write message websocket-stream))))

(defun read-length (length stream)
  (logging "Reading length...")
  (cond ((<= length 125)
         length)
        ((= length 126)
         (nibbles:read-ub16/be stream))
        ((= length 127)
         (nibbles:read-ub64/be stream))))

(defun read-mask (stream)
  (logging "Reading mask...")
  (let ((mask (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (index 4 mask)
      (setf (aref mask index) (eread-byte stream :read-mask)))))
     
(defun read-payload (length mask stream)
  (logging "Reading payload of length ~D." length)
  (let ((payload (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length payload)
      (setf (aref payload index)
            (logxor (eread-byte stream :read-payload)
                    (elt mask (mod index 4)))))))

(defgeneric handle-frame (frame websocket)
  (:documentation "Attempt to correctly handle FRAME for WEBSOCKET.")
  (:method (frame websocket)
    (error 'unsupported-frame :frame frame))
  (:method :around (frame websocket)
    (logging "Handling frame: ~A~%For Websocket: ~A~%"
             frame websocket)
    (call-next-method)))

(defgeneric close-frame-p (frame)
  (:method ((c close))
    t)
  (:method (c)
    nil))

(defmethod handle-frame ((frame ping) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (send-pong websocket (if (zerop len)
                             (make-array 0 :element-type '(unsigned-byte 8))
                             (read-payload len mask (socket-stream websocket))))))

(defmethod handle-frame ((frame pong) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (unless (zerop len)
      ;;no payload if its 0
      (read-payload len mask (socket-stream websocket)))))
      
(defmethod handle-frame ((frame text) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (let* ((payload (read-payload len mask (socket-stream websocket)))
           (data (handler-case
                     ;; payload must be valid utf-8
                     (octets-to-string payload)
                   (serious-condition ()
                     (error 'not-utf8))))
           (text (make-instance 'text-data :data data)))
      (append-stash websocket text))))

(defmethod handle-frame ((frame binary) websocket)
  (with-accessors ((len len)
                   (mask mask))
      frame
    (let* ((payload (read-payload len mask (socket-stream websocket)))
           (binary (make-instance 'binary-data :data payload)))
      (append-stash websocket binary))))
       
(defmethod read-frame ((websocket websocket))
  #.(ds "Read a from from the stream of WEBSOCKET. ~
         When the frame is the last one in a series, return the complete message. ~
         Could also return :eof, :close, :error.")
  (logging "Reading frame.")
  (let* ((stream (socket-stream websocket))
         (b0 (eread-byte stream :eof))
         (b1 (eread-byte stream :eof))                
         (fin (< 0 (logand b0 #b10000000)))
         (opcode (let ((opcode (logand b0 #b1111)))
                   (if (= opcode +continuation+)
                       (opcode websocket)
                       opcode)))
         (maskbit (or (< 0 (logand b1 #b10000000))
                   (error 'bad-mask)))
         (len (logand b1 #b1111111))
         (frame (make-frame opcode)))
    (logging "b0: ~D. b1: ~D. Fin: ~D. Opcode: ~D. Maskbit: ~D~%. Len: ~D.~%Frame: ~A."
             b0 b1 fin opcode maskbit len frame)
      ;; client close frame
    (when (close-frame-p frame)
      (return-from read-frame frame))
      ;; read length
    (let* ((len (read-length len stream))
           ;;read mask
           (mask (read-mask stream)))        
      (setf (len frame) len
            (mask frame) mask)    
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
  (logging "Sending close frame.")
  (force-write (list (+ #b10000000 +close+) 0)
               (socket-stream websocket)))

(defmethod close ((websocket closed) &rest abort)
  (declare (ignore abort))
  nil)

(defmethod close ((websocket websocket) &rest abort)
  (declare (ignore abort))
  (logging "Closing.")
  ;; send close frame
  (send-close-frame websocket)
  ;; move to closing state
  (change-class websocket 'closing)
  ;; run function
  (call-resource-function :close websocket))

(defun construct-http-response (&rest list)
  (string-to-octets (alist->header list)))

(defgeneric ready-or-closing-p (websocket)
  (:method ((r ready))
    t)
  (:method ((c closing))
    t)
  (:method (n)
    nil))

(defmethod handle-frame ((frame portal-condition) websocket)
  (cl:close (socket-stream websocket))  
  ;; close socket
  ;;spec says that we should try to read and disgard any bytes.. but says
  ;;MAY just yeet the connectionn
  (cl:close (socket-stream websocket) :abort t)
  ;; move to closing state
  (change-class websocket 'closing)
  ;; run function
  (call-resource-function :close websocket))

(defmethod handle-frame ((frame close) (websocket closing))
  (close websocket)
  (cl:close (socket-stream websocket) :abort t))
  
(defun handle-upgrade (stream request)
  (let ((websocket (make-instance 'connecting
                                  :header request
                                  :stream stream)))
    ;; send accept response header
    (force-write (construct-http-response      
                  (cons :version +http-version+)
                  (cons :code 101)
                  (cons :code-meaning "Switching Protocols")
                  (cons :upgrade "websocket")
                  (cons :connection "Upgrade")
                  (cons :sec-websocket-accept
                        (sha1-base64 (str:concat (sec-websocket-key request)
                                                 +sec-key+))))
                 stream)
    ;; custom connect function
    (call-resource-function :open websocket)
    (change-class websocket 'ready)
    ;; messages
    (read-frame-loop websocket)
    (change-class websocket 'closed)))
;; move to closed state

(defun read-frame-loop (websocket)
  (while ((ready-or-closing-p websocket))
    ;; read new payload
    (print "loopin'")
    (logging "Websocket: ~A~%" websocket)
    (multiple-value-bind (frame fin)
        (handler-case 
            (read-frame websocket)
          (portal-condition (c)
            c))
      ;;when its fin we want to call the message resource with the stash        
      (handle-frame frame websocket)
      (when fin
        (logging "Finished sequence.~%")
        (let ((stashed (get-stash websocket)))
          (clear-stash websocket)
          (call-resource-function :message websocket stashed))))))

            
(defun handle-cannot-upgrade (stream reason)
  (logging "Cannot upgrade: ~A" reason)
  (force-write (construct-http-response
                (cons :version +http-version+)
                (cons :code 400)
                (cons :code-meaning "Bad Request")
                (cons :reason reason))
               stream))

(defun websocket-handler (stream)
  ;; connect
  (let* ((request (read-headers stream)))
    (handler-case (progn (check-can-upgrade request)
                         (handle-upgrade stream request))
      (upgrade-problem (c)
        ;; headers that can't upgrade
        (handle-cannot-upgrade stream (string-downcase (key c)))))))
                         
(defun server (&optional (port 4433) multi-thread)
  (socket-server *wildcard-host* port
                 (lambda (stream)
                   (block :bail
                     (handler-bind
                         ((error
                            (lambda (condition)
                              (format *error-output* "~A~%" condition)
                              (unless *debug-on-error* (return-from :bail)))))
                       (websocket-handler stream))))
    nil
    :in-new-thread t
    :multi-threading multi-thread
    :element-type '(unsigned-byte 8)
    :name "Websocket Server"))

(defun server-close (websocket-server)
  (when (bt:thread-alive-p websocket-server)
    (logging "Stopping websocket-server.")
    (bt:destroy-thread websocket-server)))
