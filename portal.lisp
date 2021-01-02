(in-package #:portal)

(defparameter origin nil
  "If Cross-Origin-Request-Sharing is disallowed,
set to the origin (www.example.com).")

(defvar *debug-on-error* t)

(defconstant +crlf+
  (coerce (list (code-char 13) (code-char 10)) 'string))
(defconstant +sec-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
(defconstant +http-version+
  "HTTP/1.1")

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

(defun call-with-handler (handler function websocket &rest arguments)
  (restart-case
      (handler-case
          (apply function websocket arguments)
        (t (condition)
          (funcall handler websocket condition)
          (when *debug-on-error*
            (error condition))))
    (drop-client ()
      :report "close the client websocket"
      (close websocket))
    (continue ()
      :report "stop processing and continue with normal operations"
      (continue))
    (retry ()
      :report "retry processing"
      (apply #'call-with-handler handler function websocket arguments))))

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
     (let ((parts (str:split " " (first lines))))
       (append
        (list (cons :method (first parts)))
        ;; path
        (list (cons :script
                    (->> (second parts)
                      (position #\?)
                      (subseq (second parts) 0)
                      (string-downcase))))
        ;; query string
        (when (find #\? (second parts))
          (list (cons :query-string
                      (subseq (second parts)
                              (+ (position #\? (second parts)) 1)))))
        (list (cons :version (third parts)))))
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
   (ready-state :type number :reader ready-state
                :initform 0)
   (fragment-opcode :reader opcode :documentation
                    "Store the type of frame we are currently on.")
   (stash :type list :initform (list) :reader stash :documentation
          "Store the message content before all fames have arrived.")
   (sent-close-frame :initform nil :type boolean :reader sent-close-frame-p)))
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
(defmethod read-frame ((websocket websocket))
  "Read a from from the stream of WEBSOCKET.
when the frame is the last one in a series, return the complete message.
Could also return :eof, :close."
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
      (when (or (null mask)
                ;; client close frame
                (= opcode +close+))
        (return-from read-frame :close))
      (let ((len (cond
                   ((<= len 125) len)
                   ((= len 126)
                    (reduce             ;read 2 bytes as int
                     #'+ (loop for place from 1 downto 0
                               collect (* (or (read-byte stream nil)
                                              (return-from read-frame :eof))
                                          (expt 2 (* 8 place))))))
                   ((= len 127)
                    (reduce            ;read 8 bytes as int
                     #'+ (loop for place from  7 downto 0
                               collect (* (or (read-byte stream nil)
                                              (return-from read-frame :eof))
                                          (expt 2 (* 8 place)))))))))
        ;; read masking key
        (let* ((mask (coerce (loop for place from 3 downto 0
                                   collect (or (read-byte stream nil)
                                               (return-from read-frame :eof)))
                             'vector))
               ;; read payload
               (payload (loop for index from 0 below len
                              collect (logxor (or (read-byte stream nil)
                                                  (return-from read-frame :eof))
                                              (elt mask (mod index 4))))))
          ;; TODO: implement ping & pong and close
          (cond
            ;; closing frame
            ((= opcode +close+)
             (return-from read-frame :close))
            ((= opcode +ping+)
             (send-pong websocket payload))
            ;; begining a text frame
            ((= opcode +text+)
             (setf (slot-value websocket 'fragment-opcode) opcode)
             (append-stash
              websocket
              (handler-case
                  ;; payload must be valid utf-8
                  (flex:octets-to-string payload :external-format :utf-8)
                (t () (return-from read-frame :close)))))
            ;; begining a binary frame
            ((= opcode +binary+)
             (setf (slot-value websocket 'fragment-opcode) opcode)
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
     (+ #b10000000                      ;fin
        (if binary                      ;opcode
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
      ;; extended twice
      ((< (expt 2 16) len)
       (loop for place from 7 downto 0 do
         (write-byte (logand len (* 15 (expt 2 (* 4 place))))
                     stream)))
      ;; extended once
      ((<= (expt 2 16) len)
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
(defmethod send-close-frame ((websocket websocket))
  (write-sequence
   (list (+ #b10000000 +close+) 0)
   (socket-stream websocket))
  (force-output (socket-stream websocket))
  (setf (slot-value websocket 'sent-close-frame) t)
  (setf (slot-value websocket 'ready-state) +closing+))
(defmethod close ((websocket websocket) &rest abort)
  (declare (ignore abort))
  (when (= (ready-state websocket)
           +closed+)
    (return-from close))
  (if (= (ready-state websocket)
         +closing+)
      ;; in closing state
      ;; run custom function
      (destructuring-bind (connect message disconnect error)
          (-<>> websocket
            (header)
            (assoc :script)
            (cdr)
            (assoc <> path-handlers
                   :test #'str:starts-with-p)
            (cdr))
        (declare (ignore connect message))
        (call-with-handler error disconnect websocket)
        (if (sent-close-frame-p websocket)
            ;; client initiated close
            ;; send response close frame
            (send-close-frame websocket)
            ;; server initiated close
            ;; close TCP socket
            (cl:close (socket-stream websocket)))
        (setf (slot-value websocket 'ready-state)
              +closed+))
      ;; just send close frame to enter closing state
      (send-close-frame websocket)))

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
            (let ((websocket (make-instance
                              'websocket
                              :header header
                              :stream stream)))
              ;; start connecting
              (setf (slot-value websocket 'ready-state)
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
              (force-output stream)
              ;; custom connect function
              (call-with-handler error connect websocket)
              (setf (slot-value websocket 'ready-state)
                    +ready+)
              ;; messages
              (loop while (member (ready-state websocket)
                                  (list +ready+ +closing+))
                    for payload = (read-frame websocket)
                    while (not (eq payload :eof))
                    if (eq payload :close) do
                      (close websocket)
                    else if payload do
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
            (force-output stream))))))

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
