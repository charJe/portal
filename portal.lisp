(ql:quickload '(usocket-server arrows str sha1 flexi-streams))

(defpackage portal
  (:nicknames :pws)
  (:use #:cl #:usocket #:arrows)
  (:shadow :close)
  (:export :define-path-handler
           :websocket
           :send
           :close
           :websocket-server
           :websocket-server-close
           :*debug-on-error*))
(in-package #:portal)

(defparameter origin nil
  "If Cross-Origin-Request-Sharing is disallowed,
set to the origin (www.example.com).")

(defvar *debug-on-error* t)

(defconstant +crlf+
  (coerce (list (code-char 13) (code-char 10)) 'string))

(defconstant +sec-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defconstant +http-version+ "HTTP/1.1")

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
(defconstant +closing+ 2)
(defconstant +closed+ 3)

(defparameter path-handlers (list)
  "Alist of path handler functions.
Key: path, Value: list of handler functions")

(defun define-path-handler
    (path &key
            (connect (lambda (websocket)
                       (declare (ignore websocket))))
            (message (lambda (websocket message)
                       (declare (ignore websocket message))))
            (disconnect (lambda (websocket)
                          (declare (ignore websocket))))
            (error (lambda (websocket condition)
                     (declare (ignore websocket condition)))))
  (if (assoc (string-downcase path)
             path-handlers)
      (setf (cdr (assoc (string-downcase path)
                        path-handlers))
            (list connect
                  message
                  disconnect
                  error))
      (setf path-handlers
            (acons (string-downcase path)
                   (list connect
                         message
                         disconnect
                         error)
                   path-handlers))))

(defun starts-with (start list)
  (cond
    ((null start) t)
    ((equal (first start) (first list))
     (starts-with (rest start) (rest list)))))

(defmacro call-with-handler (handler function &rest arguments)
  (let ((condition (gensym)))
    `(handler-case
         (funcall ,function ,@arguments)
       (t (,condition)
         (funcall ,handler (first ',arguments) ,condition)
         (when *debug-on-error*
           (error ,condition))))))

(defun read-until (string &optional (stream *standard-input*))
  (loop with test = (map 'list 'char-code (reverse string))
        with result = (list)
        for byte = (read-byte stream nil nil)
        if (null byte)
          return (when result
                   (map 'string #'code-char (reverse result)))
        do (push byte result)
        if (starts-with test result)
          return (map 'string #'code-char (reverse result))))

(defun header->alist (string)
  "Return the alist corresponding to the request header STRING."
  (let ((lines (str:split +crlf+ string :omit-nulls t)))
    (append
     (->> lines
       (first)
       (str:split " ")
       (map 'list #'cons (list :method :script :version)))
     (->> lines
      (rest)
      (map 'list
           (lambda (line)
             (when (string/= line +crlf+)
               (destructuring-bind (name part) (str:split ": " line)
                 (cons (intern (string-upcase name)
                               :keyword)
                       part)))))
      (remove-if #'null)))))

(defun alist->header (alist)
  "Return the string response header corresponding to ALIST."
  (-<>> (format nil "~a ~a ~a~a"
               (cdr (assoc :version alist))
               (cdr (assoc :code alist))
               (cdr (assoc :code-meaning alist))
               +crlf+)
    (reduce
     (lambda (header item)
       (if (member (car item) (list :version :code :code-meaning))
           header
           (format nil "~a~a: ~a~a"
                   header
                   (->> item
                     (car)
                     (symbol-name))
                   (cdr item)
                   +crlf+)))
     alist
     :initial-value)
    (concatenate 'string <> +crlf+)))

(defun can-upgrade-p (header)
  "True if the alist HEADER has qualified to be upgraded to a websocket.
If nil, the second value will be the reason."
  (handler-case
      (and
       ;; method = get
       (or (->> header
             (assoc :method)
             (cdr)
             (string-upcase)
             (string= "GET"))
           (values nil :method))
       ;; http version greater than 1
       (or (->> header
             (assoc :version)
             (cdr)
             (str:split "/")
             (second)
             (read-from-string)
             (<= 1.1))
           (values nil :version))
       ;; missing sec-websocket-key
       (or (assoc :sec-websocket-key header)
           (values nil :sec-websocket-key))
       ;; cors
       (if origin
           (or (->> header
                 (assoc :origin)
                 (cdr)
                 (string= origin))
               (values nil :origin))
           t)
       ;; trying to upgrade
       (and (or (->> header
                  (assoc :upgrade)
                  (cdr)
                  (string= "websocket"))
                (values nil :upgrade))
            (or (->> header
                  (assoc :connection)
                  (cdr)
                  (str:containsp "Upgrade"))
                (values nil :connection)))
       ;; script
       (or (-<>> header
             (assoc :script)
             (cdr)
             (assoc <> path-handlers
                    :test #'str:starts-with-p))
           (values nil :script)))
    (t () (values nil :request))))

(defclass websocket ()
  ((header :type list :initarg :header :reader header)
   (stream :type stream :initarg :stream :accessor socket-stream)
   (ready-state :type number :accessor ready-state
                :initform 0)
   (fragment-opcode :accessor opcode :documentation
                    "Store the type of frame we are currently on.")
   (stash :type list :initform (list) :reader stash :documentation
          "Store the message content before all fames have arrived.")))
(defmethod append-stash ((websocket websocket) sequence)
  (with-slots (stash) websocket
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
    (setf stash (list))))
(defmethod read-frame ((websocket websocket))
  "Read a from from the stream of WEBSOCKET.
when the frame is the last one in a series, return the complete message.
Could also return :eof, :close."
  (let* ((stream (socket-stream websocket))
         (b0 (read-byte stream nil :eof)))
    (when (eq :eof b0)
      (return-from read-frame :eof))
    (let* ((b1 (read-byte stream))
           (fin (< 0 (logand b0 #b10000000)))
           (opcode (let ((opcode (logand b0 #b1111)))
                     (if (= opcode +continuation+)
                         (opcode websocket)
                         opcode)))
           (mask (< 0 (logand b1 #b10000000)))
           (len (logand b1 #b1111111)))
      (setf (opcode websocket) opcode)
      ;; A server MUST close the connection upon receiving a frame with
      ;; the MASK bit set to 0.
      (when (null mask)
        (return-from read-frame :close))
      (let ((len (cond
                   ((<= len 125) len)
                   ((= len 126)
                    (reduce             ;read 2 bytes as int
                     #'+ (loop for place from 1 downto 0
                               collect (* (read-byte stream)
                                          (expt 2 (* 8 place))))))
                   ((= len 127)
                    (reduce            ;read 8 bytes as int
                     #'+ (loop for place from  7 downto 0
                               collect (* (read-byte stream)
                                          (expt 2 (* 8 place)))))))))
        ;; read masking key
        (let* ((mask (coerce (loop for place from 3 downto 0
                                   collect (read-byte stream))
                             'vector))
               ;; read payload
               (payload (loop for index from 0 below len
                              collect (logxor (read-byte stream)
                                              (elt mask (mod index 4))))))
          ;; TODO: implement ping & pong and close
          (cond
            ;; begining a text frame
            ((= opcode +text+)
             (append-stash
              websocket
              (handler-case (flex:octets-to-string payload :external-format :utf-8)
                ;; payload must be valid utf-8
                (t () (return-from read-frame :close)))))
            ;; begining a binary frame
            ((= opcode +binary+)
             (append-stash websocket payload))
            (:else
             (return-from read-frame :close)))
          ;; return message if all frames have been processed
          (when fin
            (prog1 (get-stash websocket)
              (clear-stash websocket))))))))
(defmethod send ((websocket websocket) message &optional binary)
  (let* ((stream (socket-stream websocket))
         (message (if (typep message '(or string (array (unsigned-byte 8))))
                      message
                      (format nil "~a" message)))
         (len (length message)))
    (declare (type number len))
     (write-byte
      (+ #b10000000                     ;fin
         (if binary                   ;opcode
             +binary+
             +text+))
      stream)
    (write-byte
     (+ #b00000000                      ;mask
        (cond                           ;length
          ((<= len 125) len)
          ((<= len (expt 2 16)) 126)
          ((< (expt 2 16) len) 127)))
     stream)
    ;; extended length length
    (cond
      ((< (expt 2 16) len)              ;most extended
       (loop for place from 7 downto 0 do
         (write-byte (logand len (* 15 (expt 2 (* 4 place))))
                     stream)))
      ((<= (expt 2 16) len)             ;extended
       (loop for place from 7 downto 0 do
         (write-byte (logand len (* 15 (expt 2 (* 4 place))))
                     stream))))
    ;; payload
    (write-sequence
     (if binary
         message
         (flex:string-to-octets message :external-format :utf-8))
     stream)
    (force-output stream)
    message))
(defmethod close ((websocket websocket) &rest abort)
  (declare (ignore abort))
  (destructuring-bind (connect message disconnect error)
      (-<>> websocket
        (header)
        (assoc :script)
        (cdr)
    (declare (ignore connect message))
    (setf (ready-state websocket)
          +closing+)
    ;; TODO: send disconnect frame
    (setf (ready-state websocket)
          +closed+)))
            (assoc <> path-handlers
                   :test #'str:starts-with-p)
            (cdr))
        (call-with-handler error disconnect websocket)

(defun websocket-handler (stream)
  ;; connect
  (let ((header (header->alist (read-until (str:concat +crlf+ +crlf+)
                                           stream))))
    (multiple-value-bind (can-upgrade reason)
        (can-upgrade-p header)
      (if can-upgrade
          (destructuring-bind (connect message disconnect error)
              (-<>> header
                (assoc :script)
                (cdr)
                (assoc <> path-handlers
                       :test #'str:starts-with-p)
                (cdr))
            (declare (ignore disconnect))
            (let ((websocket (make-instance 'websocket
                                             :header header
                                             :stream stream)))
              ;; start connecting
              (setf (ready-state websocket)
                    +connecting+)
              ;; send accept response header
              (write-sequence
               (flex:string-to-octets
                (alist->header
                 (append (list
                          (cons :version +http-version+)
                          (cons :code 101)
                          (cons :code-meaning "Switching Protocols")
                          (cons :upgrade "websocket")
                          (cons :connection "Upgrade")
                          (cons :sec-websocket-accept
                                (-<>> header
                                  (assoc :sec-websocket-key)
                                  (cdr)
                                  (str:concat <> +sec-key+)
                                  (sha1:sha1-base64))))))
                :external-format :utf-8)
               stream)
              ;; custom connect function
              (setf (ready-state websocket)
              (call-with-handler error connect websocket)
                    +ready+)
              ;; messages
              (loop while (= +ready+ (ready-state websocket))
                    with stream = stream
                    for payload = (read-frame websocket)
                    while (not (member payload (list :eof :close)))
                    if payload
              ;; disconnect
              (close websocket)
                      (call-with-handler error message websocket payload))))
          ;; headers that can't upgrade
          (progn
            (write-sequence
             (flex:string-to-octets
              (alist->header
               (list
                (cons :version +http-version+)
                (cons :code 400)
                (cons :code-meaning "Bad Request")
                (cons :reason reason)))
              :external-format :utf-8)
             stream)
            (force-output stream))))))))

(defun websocket-server (&optional (port 4433) multi-thread)
  (socket-server *wildcard-host* port
                 (lambda (stream)
                   (websocket-handler stream))
                 nil
                 :in-new-thread t
                 :multi-threading multi-thread
                 :element-type '(unsigned-byte 8)
                 :name "Websocket Server"))

(defun websocket-server-close (websocket-server)
  (bt:destroy-thread websocket-server))

(assert (equal (header->alist
                (str:join +crlf+ '("GET /chat HTTP/1.1"
                                   "Host: example.com:8000"
                                   "Upgrade: websocket"
                                   "Connection: Upgrade"
                                   "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="
                                   "Sec-WebSocket-Version: 13" "")))
               '((:METHOD . "GET") (:SCRIPT . "/chat") (:VERSION . "HTTP/1.1")
                 (:HOST . "example.com:8000") (:UPGRADE . "websocket")
                 (:CONNECTION . "Upgrade") (:SEC-WEBSOCKET-KEY . "dGhlIHNhbXBsZSBub25jZQ==")
                 (:SEC-WEBSOCKET-VERSION . "13"))))

(assert (string= (alist->header
                  '((:version . "HTTP/1.1")
                    (:code . "101")
                    (:code-meaning . "Switching Protocols")
                    (:upgrade . "websocket")
                    (:connection . "Upgrade")
                    (:sec-websocket-accept . "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")))
                 (concatenate 'string (str:join +crlf+
                         '("HTTP/1.1 101 Switching Protocols"
                           "UPGRADE: websocket"
                           "CONNECTION: Upgrade"
                           "SEC-WEBSOCKET-ACCEPT: s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))
                             +crlf+ +crlf+)))

(define-path-handler "/add1"
  :connect (lambda (socket)
             (send socket "Welcome to add1 server."))
  :message (lambda (socket message)
             (send socket
                   (->> message
                     (parse-integer)
                     (1+)))))

(define-path-handler "/echo"
  :connect (lambda (socket)
             (send socket "Welcome to echo server."))
  :message (lambda (socket message)
             (send socket message)))

(define-path-handler "/no"
  :connect (lambda (socket)
             (close socket)))
