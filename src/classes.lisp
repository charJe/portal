(in-package #:portal)

(defclass websocket ()
  ((header
    :accessor header
    :initarg :header
    :type fast-http:http-request
    :documentation "HTTP header for websocket upgrade.")
   (stream
    :accessor socket-stream
    :initarg :stream
    :type stream)
   (opcode
    :accessor opcode
    :initarg :opcode
    :type (unsigned-byte 8)
    :documentation "The current working OPCODE.")
   (path
    :accessor path
    :initarg :path
    :type pathname
    :documentation "The resource from header as a pathname")
   (continuation-type
    :accessor continuation-type
    :initarg :continuation-type
    :initform nil
    :type (or null class)
    :documentation
    #.(ds "When a data-frame is received which doesn't contain a fin, this means we are ~
           dealing with fragmented data. The continuation type is set to the type of ~
           frame for the first type which initiates the fragmentation."))))

;; socket ready state
(defconstant +connecting+ 0)
(defconstant +ready+ 1)
(defconstant +closing+ 2
  "When the first closing frame has been sent or received.")
(defconstant +closed+ 3)

(defclass connecting (websocket)
  ()
  (:documentation "When the websocket is connecting."))

(defclass ready (websocket)
  ()
  (:documentation "When the websocket is ready."))

(defclass closing (websocket)
  ()
  (:documentation "When the websocket is closing."))

(defclass closed (websocket)
  ()
  (:documentation "When the websocket is closed."))

(defclass frame ()
  ((op
    :initarg :op
    :type (unsigned-byte 8))
   (mask
    :accessor mask 
    :initarg :mask
    :type (simple-array (unsigned-byte 8) (4)))
   (length
    :accessor len
    :initarg :length
    :initform 0
    :type number)
   (fin
    :accessor fin
    :initarg :fin
    :initform nil
    :type boolean)))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :type t)
    (format stream "op: #x~x. Mask: ~A. Length: ~D."
            (slot-value frame 'op)
            (if (slot-boundp frame 'mask)
                (mask frame)
                "NOT SET")
            (if (slot-boundp frame 'length)
                (len frame)
                0))))

;; frame control codes
(defconstant +continuation+ #x0)
(defconstant +text+ #x1)
(defconstant +binary+ #x2)
(defconstant +close+ #x8)
(defconstant +ping+ #x9)
(defconstant +pong+ #xA)

(defparameter *frame->class* (make-hash-table :test #'eql :size 6))

(defmacro defframe (name code (&optional (super 'frame)) slots doc)
  `(let ((class
           (defclass ,name ,(list super)
             ,slots
             (:documentation ,doc)
             (:default-initargs :op ,code))))
     (setf (gethash ,code *frame->class*) class)))

(defun make-frame (code &key keys &allow-other-keys)
  (let ((from-hash? (gethash code *frame->class* nil)))
    (apply #'make-instance 
           (or from-hash?
               (cond ((<= #x3 code #x7)
                      'reserved-non-control-frame)
                     ((<= #xB code #xF)
                      'reserved-control-frame)
                     (t (error 'unknown-frame-op))))
           keys)))

(defclass has-payload ()
  ((payload
    :accessor payload
    :initarg :payload
    :initform nil 
    :type (or null (array (unsigned-byte 8) (*))))))

(defclass control-frame (frame has-payload)
  ())

(define-constant +control-frame-max-payload-size+ 125
  :test #'=)

(defclass data-frame (frame has-payload)
  ())

(defgeneric data-frame-p (frame)
  (:method ((frame data-frame))
    t)
  (:method (frame)
    nil))
                                     
(defframe continuation #x0 ()
          ()          
  "Continuation frame")

(defframe text #x1 (data-frame)
  ((data
    :accessor data
    :type string))
  "text frame")

(defframe binary #x2 (data-frame)
  ((data
    :accessor data
    :type (array (unsigned-byte 8) (*))))
  "Binary frame")

(defframe close #x8 (control-frame)
          ((code
            :accessor code
            :initarg :code 
            :initform nil 
            :type (or null (unsigned-byte 16)))
           (reason
            :accessor reason
            :initarg :reason
            :initform nil
            :type (or null string)))           
  "Close frame")

(defframe ping #x9 (control-frame)
  ()
  "Ping frame")

(defframe pong #xA (control-frame)
  ()
  "Pong frame")

(defclass reserved-frame (frame)
  ())

(defclass reserved-control-frame (reserved-frame)
  ()
  (:documentation "The frames between #xB-F"))

(defclass reserved-non-control-frame (reserved-frame)
  ()
  (:documentation "The reserved control frames between #x3-7"))

(deftype pathname-list () `(satisfies pathname-list-p))

(defun pathname-list-p (list)
  (every #'pathnamep list))

(defclass server ()
  ((websocket
    :accessor websocket
    :initarg :websocket
    :type websocket
    :documentation "An instance of websocket.")
   (port
    :accessor port
    :initarg :port
    :type (unsigned-byte 16)
    :documentation "The listening port number.")
   (thread
    :accessor thread
    :initarg :thread
    :type bt:thread
    :documentation "The tcp server.")
   (paths
    :accessor paths
    :initarg :paths
    :type pathname-list
    :documentation "List of pathnames")
   (cap
    :accessor cap
    :initarg :cap
    :type (or boolean fixnum)
    :documentation "Either nil or a fixnum used to denote maximum size of stash.")
   (origins 
    :accessor origins 
    :initarg :origins 
    :initform ()
    :type list
    :documentation "A list of acceptable origins for websockets."))
  (:documentation "A listening websocket server."))


(defparameter *status-codes* ()
  "Plist of valid key->status-codes.")

(defmacro defstatus-code ((key code))
  `(push (cons ,key ,code) *status-codes*))

(defstatus-code (:NORMAL 1000))
(defstatus-code (:GOING-AWAY 1001))
(defstatus-code (:PROTOCOL-ERROR 1002))
(defstatus-code (:INVALID-TYPE 1003))
(defstatus-code (:INCONSISTENT-TYPE 1007))
(defstatus-code (:POLICY-VIOLATION 1008))
(defstatus-code (:OVERSIZE 1009))
(defstatus-code (:EXTENSION-NEGOTIATION-FAILURE 1010))
(defstatus-code (:FATAL 1011))

(defgeneric check-valid-status-code (code)
  (:documentation
   #.(ds "Uses CODE to check if its a usable status code. ~
         If invalid signals 'invalid-status-code. ~
         Continue restart is established to ignore this check, can also specialize the GF"))
  (:method :around (code)
    (restart-case
        (or (call-next-method)
            (error 'invalid-status-code
                   :key code))
      (continue ()
        :report "Invalid code but continue anyway?"
        t)))
  (:method ((code symbol))
    (assoc code *status-codes* :test #'eq))
  (:method ((code number))
    (rassoc code *status-codes* :test #'=)))

(defgeneric code-value (code)
  (:method ((code number))
    code)
  (:method ((code symbol))
    (cdr (assoc code *status-codes* :test #'eq))))

