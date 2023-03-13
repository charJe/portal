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
    :type (or null frame)
    :documentation
    #.(ds "When a data-frame is received which doesn't contain a fin, this means we are ~
           dealing with fragmented data. The continuation type is set to the type of ~
           frame for the first type which initiates the fragmentation."))
   (stash
    :accessor stash
    :initarg :stash
    :type stash
    :documentation "Store the message content before all fames have arrived.")))

(defclass stash ()
  ((stash
    :accessor stash
    :initarg :stash
    :initform nil
    :type (or null flexi-streams:flexi-output-stream))))

(defgeneric capped-stash-p (stash)
  (:method ((capped-stash stash))
    t)
  (:method (c)
    nil))

(defclass uncapped-stash (stash)
  ())

(defclass capped-stash (stash)
  ((cap
    :accessor cap
    :initform 0
    :initarg :cap
    :type fixnum
    :documentation "Maximum size of the stash. Stops OOMs.")))

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
    :type number)))

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

(defun make-frame (code)
  (let ((from-hash? (gethash code *frame->class* nil)))
    (make-instance
     (or from-hash?
         (cond ((<= #x3 code #x7)
                'reserved-non-control-frame)
               ((<= #xB code #xF)
                'reserved-control-frame)
               (t (error 'unknown-frame-op)))))))

(defclass control-frame (frame)
  ())

(define-constant +control-frame-max-payload-size+ 125
  :test #'=)

(defclass data-frame (frame)
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
  ()
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
   (stash-cap
    :accessor stash-cap
    :initarg :stash-cap
    :type (or boolean fixnum)
    :documentation "Either nil or a fixnum used to denote maximum size of stash.")
   (origins 
    :accessor origins 
    :initarg :origins 
    :initform ()
    :type list
    :documentation "A list of acceptable origins for websockets."))
  (:documentation "A listening websocket server."))
