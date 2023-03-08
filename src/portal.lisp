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

;; socket ready state
(defconstant +connecting+ 0)
(defconstant +ready+ 1)
(defconstant +closing+ 2
  "When the first closing frame has been sent or received.")
(defconstant +closed+ 3)

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
                   (resource fast-http:http-resource))
      http-request
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
               (or (string= *origin* (origin http-request))
                   (c :origin))
               t)
           ;; trying to upgrade
           (or (string= "websocket" (upgrade http-request))
               (c :upgrade))
           (or (str:containsp "Upgrade" (connection http-request))
               (c :connection))
           ;; script
           (or (get-resource resource)
               (c :script))
           t)
        (t () (c :request))))))

(defclass websocket ()
  ((header
    :reader header
    :initarg :header
    :type fast-http:http-request
    :documentation "HTTP header for websocket upgrade.")
   (stream
    :accessor socket-stream
    :initarg :stream
    :type stream)
   (ready-state
    :reader ready-state
    :initform 0
    :type number)
   (fragment-opcode
    :reader opcode
    :documentation  "Store the type of frame we are currently on.")
   (stash
    :reader stash
    :initform ()
    :type list
    :documentation "Store the message content before all fames have arrived.")))

(defmethod append-stash ((websocket websocket) sequence)
  (with-slots (stash)
      websocket
    (push sequence stash)))

(defmethod get-stash ((websocket websocket))
  (with-slots (stash fragment-opcode) websocket
    (apply #'concatenate
           (cond
             ((= fragment-opcode +text+) 'string)
             ((= fragment-opcode +binary+) 'vector))
           (reverse stash))))

(defmethod clear-stash ((websocket websocket))
  (with-slots (stash) websocket
    (setf stash ())))

(defmethod send-pong ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (when (< 125 (length message))
    (error "Message cannot be greater than 125 bytes"))
  (with-slots (stream) websocket
    (write-sequence
     (list (+ #b10000000 +pong+)
           (length message))
     stream)
    (write-sequence message stream)
    (force-output stream)))

(defmethod send-ping ((websocket websocket) message)
  (declare (type (array (unsigned-byte 8)) message))
  (when (< 125 (length message))
    (error "Message cannot be greater than 125 bytes"))
  (with-slots (stream) websocket
    (write-sequence
     (list (+ #b10000000 +ping+) (length message))
     stream)
    (write-sequence message stream)
    (force-output stream)))

(defun read-length (length stream)
  (cond ((<= length 125)
         length)
        ((= length 126)
         (nibbles:read-ub16/be stream))
        ((= length 127)
         (nibbles:read-ub64/be stream))))

(defun read-mask (stream)
  (let ((mask (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (index 4 mask)
      (setf (aref mask index) (read-byte stream nil)))))
     
(defun read-payload (length mask stream)
  (let ((payload (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length payload)
      (setf (aref payload index)
            (logxor (or (read-byte stream nil)
                        (return-from read-payload :eof));;I am gonna replace these
                    ;;with conditions
                    (elt mask (mod index 4)))))))

(defmethod read-frame ((websocket websocket))
  #.(ds "Read a from from the stream of WEBSOCKET. ~
         When the frame is the last one in a series, return the complete message. ~
         Could also return :eof, :close, :error.")
  (let* ((stream (socket-stream websocket))
         (b0 (read-byte stream nil :eof)))
    (when (eq :eof b0)
      (return-from read-frame :eof))
    (let* ((b1 (or (read-byte stream nil)
                   (return-from read-frame :eof)))
           (fin (< 0 (logand b0 #b10000000)))
           (opcode (let ((opcode (logand b0 #b1111)))
                     (if (= opcode +continuation+)
                         (opcode websocket)
                         opcode)))
           (mask (< 0 (logand b1 #b10000000)))
           (len (logand b1 #b1111111)))
      ;; A server MUST close the connection upon receiving a frame with
      (unless mask
        (return-from read-frame :error))
      ;; client close frame
      (when (= opcode +close+)
        (return-from read-frame :close))
      ;; read length
      (let* ((len (read-length len stream))
             ;;read mask
             (mask (read-mask stream))
             (payload (read-payload len mask stream)))
        ;; read payload
        (cond
          ((= opcode +ping+)
           (send-pong websocket payload))
          ;; begining a text frame
          ((= opcode +text+)
           (setf (slot-value websocket 'fragment-opcode) opcode)
           (append-stash
            websocket
            (handler-case
                ;; payload must be valid utf-8
                (octets-to-string payload)
              (t () (return-from read-frame :error)))))
          ;; begining a binary frame
          ((= opcode +binary+)
           (setf (slot-value websocket 'fragment-opcode) opcode)
           (append-stash websocket payload))
          (t
           (return-from read-frame :error)))
        ;; return message if all frames have been processed
        (when fin
          (prog1 (get-stash websocket)
            (clear-stash websocket)))))))


(defun write-length (stream length)
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
    (write-sequence message stream)
    (force-output stream)
    message))

(defmethod send-close-frame ((websocket websocket))
  (write-sequence
   (list (+ #b10000000 +close+) 0)
   (socket-stream websocket))
  (force-output (socket-stream websocket)))

(defmethod close ((websocket websocket) &rest abort)
  (declare (ignore abort))
  (when (= (ready-state websocket)
           +closed+)
    (return-from close))
  ;; send close frame
  (send-close-frame websocket)
  ;; move to closing state
  (setf (slot-value websocket 'ready-state)
        +closing+)
  ;; run function
  (call-resource-function :close websocket))

(defun construct-http-response (&rest list)
  (string-to-octets (alist->header list)))

(defun write-response (stream response)
  (write-sequence response stream)
  (force-output stream))

(defun handle-upgrade (stream request)
  (let ((websocket (make-instance
                    'websocket
                    :header request
                    :stream stream)))
    ;; start connecting
    (setf (slot-value websocket 'ready-state)
          +connecting+)
    ;; send accept response header
    (write-response stream (construct-http-response      
                            (cons :version +http-version+)
                            (cons :code 101)
                            (cons :code-meaning "Switching Protocols")
                            (cons :upgrade "websocket")
                            (cons :connection "Upgrade")
                            (cons :sec-websocket-accept
                                  (sha1-base64 (str:concat (sec-websocket-key request)
                                                           +sec-key+)))))
    ;; custom connect function
    (call-resource-function :open websocket)
    (setf (slot-value websocket 'ready-state)
          +ready+)
    ;; messages
    (loop while (member (ready-state websocket)
                        (list +ready+ +closing+))
          ;; read new payload
          for payload = (read-frame websocket)
          ;; close condition
          if (eq payload :eof) do
            (progn
              ;; close socket
              (cl:close (socket-stream websocket) :abort t)
              ;; move to closing state
              (setf (slot-value websocket 'ready-state)
                    +closing+)
              ;; run function
              (call-resource-function :close websocket))
          else if (eq payload :error) do
            ;; send close frame
            ;; run function
            (close websocket)
          else if (eq payload :close) do
            (if (= (ready-state websocket)
                   +closing+)
                ;; close socket
                (cl:close (socket-stream websocket) :abort t)
                        ;;; else
                ;; send close frame
                ;; run function
                (close websocket))
            ;; exit if we closed
          while (not (keywordp payload))
          ;; call custom function
          if payload do
            (call-resource-function :message websocket payload))
    ;; move to closed state
    (setf (slot-value websocket 'ready-state)
          +closed+)))

(defun handle-cannot-upgrade (stream reason)
  (write-response stream (construct-http-response
                          (cons :version +http-version+)
                          (cons :code 400)
                          (cons :code-meaning "Bad Request")
                          (cons :reason reason)))
  (force-output stream))

(defun websocket-handler (stream)
  ;; connect
  (let* ((request (read-headers stream)))
    (handler-case (progn (check-can-upgrade request)
                         (handle-upgrade stream request))
      (upgrade-problem (c)
        ;; headers that can't upgrade
        (handle-cannot-upgrade stream (string-downcase (key c)))))))
                         
(defun server (&optional (port 4433) multi-thread)
  (socket-server
   *wildcard-host* port
   (lambda (stream)
     (block nil
       (handler-bind
           ((error
              (lambda (condition)
                (format *error-output* "~A~%" condition)
                (unless *debug-on-error* (return)))))
         (websocket-handler stream))))
   nil
   :in-new-thread t
   :multi-threading multi-thread
   :element-type '(unsigned-byte 8)
   :name "Websocket Server"))

(defun server-close (websocket-server)
  (when (bt:thread-alive-p websocket-server)
    (bt:destroy-thread websocket-server)))
