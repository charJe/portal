(in-package #:portal)

(define-global-parameter -origin- nil
  "If Cross-Origin-Request-Sharing is disallowed,
set to the origin (www.example.com).")

(defvar *debug-on-error* t)

(define-constant +crlf+
    (coerce (list (code-char 13) (code-char 10)) 'string)
  :test #'string=)
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

(define-global-var -resource-handlers- (list)
  "Alist of resource handler functions.
Key: path, Value: list of handler functions")

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
        (resource (assoc (string-downcase path) -resource-handlers-
                         :test #'string=)))
    (if resource
        (setf (cdr resource) funlist)
        (setf -resource-handlers-
              (acons (string-downcase path)
                     funlist
                     -resource-handlers-)))))

(defun starts-with (start list)
  (cond
    ((null start) t)
    ((equal (first start) (first list))
     (starts-with (rest start) (rest list)))))

(defun sha1-base64 (string)
  (let ((sha1 (ironclad:make-digest 'ironclad:sha1))
        (bin-data (ironclad:ascii-string-to-byte-array string)))
    (ironclad:update-digest sha1 bin-data)
    (cl-base64:usb8-array-to-base64-string
     (ironclad:produce-digest sha1))))

(defun call-resource-function (function-name websocket &rest arguments)
  (restart-case
      (let* ((functions
               (-<>> websocket
                 (header)
                 (assoc :script)
                 (cdr)
                 (assoc <> -resource-handlers-
                        :test #'str:starts-with-p)
                 (cdr)))
             (function (elt functions
                            (case function-name
                              (:open 0)
                              (:message 1)
                              (:close 2)
                              (otherwise 3)))))
        (handler-bind
            ((error
               (lambda (condition)
                 (funcall (eval (elt functions 3)) websocket condition)
                 (unless *debug-on-error*
                   (continue condition)))))
          ;; custom open function
          (apply (eval function) websocket arguments)))
    (drop-client ()
      :report "close the client websocket"
      (close websocket))
    (continue ()
      :report "stop processing and continue with normal operations"
      (continue))
    (retry ()
      :report "retry processing"
      (apply #'call-resource-function function-name websocket arguments))))

(defun read-until (string &optional (stream *standard-input*))
  (loop with test = (map 'list 'char-code (reverse string))
        with result = (list)
        for byte = (handler-case (read-byte stream nil nil)
                     (t () nil))
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
             (parse-float)
             (<= 1.1))
           (values nil :version))
       ;; missing sec-websocket-key
       (or (assoc :sec-websocket-key header)
           (values nil :sec-websocket-key))
       ;; cors
       (if -origin-
           (or (->> header
                 (assoc :origin)
                 (cdr)
                 (string= -origin-))
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
             (assoc <> -resource-handlers-
                    :test #'str:starts-with-p))
           (values nil :script)))
    (t () (values nil :request))))

(defclass websocket ()
  ((header :type list :initarg :header :reader header :documentation
           "HTTP header for websocket upgrade.")
   (stream :type stream :initarg :stream :accessor socket-stream)
   (ready-state :type number :reader ready-state
                :initform 0)
   (fragment-opcode :reader opcode :documentation
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
Could also return :eof, :close, :error."
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
                  (flex:octets-to-string payload :external-format :utf-8)
                (t () (return-from read-frame :error)))))
            ;; begining a binary frame
            ((= opcode +binary+)
             (setf (slot-value websocket 'fragment-opcode) opcode)
             (append-stash websocket payload))
            (:else
             (return-from read-frame :error)))
          ;; return message if all frames have been processed
          (when fin
            (prog1 (get-stash websocket)
              (clear-stash websocket))))))))
(defmethod send ((websocket websocket) message)
  (let* ((stream (socket-stream websocket))
         (binary (typep message '(array (unsigned-byte 8))))
         (message (if (typep message '(or string (array (unsigned-byte 8))))
                      message
                      (format nil "~a" message)))
         (message (if binary
                      message
                      (flex:string-to-octets message :external-format :utf-8)))
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
       (loop for place from 7 downto 0
             for shift = (expt 2 (* 8 place))
             do (write-byte
                 (/ (logand len (* #b11111111 shift))
                    shift)
                 stream)))
      ;; extended once
      ((<= 125 len)
       (loop for place from 1 downto 0
             for shift = (expt 2 (* 8 place))
             do (write-byte
                 (/ (logand len (* #b11111111 shift))
                    shift)
                 stream))))
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

(defun websocket-handler (stream)
  ;; connect
  (let ((header (header->alist (read-until (str:concat +crlf+ +crlf+)
                                           stream))))
    (multiple-value-bind (can-upgrade reason)
        (can-upgrade-p header)
      (if can-upgrade
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
                                (sha1-base64))))))
              :external-format :utf-8)
             stream)
            (force-output stream)
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
                      (cl:close (socket-stream websocket))
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
                        (cl:close (socket-stream websocket))
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
                  +closed+))
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

(defun server (&optional (port 4433) multi-thread)
  (socket-server *wildcard-host* port
                 (lambda (stream)
                   (websocket-handler stream))
                 nil
                 :in-new-thread t
                 :multi-threading multi-thread
                 :element-type '(unsigned-byte 8)
                 :name "Websocket Server"))

(defun server-close (websocket-server)
  (bt:destroy-thread websocket-server))
