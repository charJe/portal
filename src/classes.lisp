(in-package #:portal)

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
   (fragment-opcode
    :reader opcode
    :documentation  "Store the type of frame we are currently on.")
   (stash
    :reader stash
    :initform ()
    :type list
    :documentation "Store the message content before all fames have arrived.")))

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

(defclass data ()
  ((data
    :reader data
    :initarg :data)))

(defclass binary-data (data)
  ((data
    :type (array (unsigned-byte 8) (*)))))

(defclass text-data (data)
  ((data
    :type string)))


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
    (format stream "op: #x~x"
            (slot-value frame 'op))))

;; frame control codes
(defconstant +continuation+ #x0)
(defconstant +text+ #x1)
(defconstant +binary+ #x2)
(defconstant +close+ #x8)
(defconstant +ping+ #x9)
(defconstant +pong+ #xA)

(defparameter *frame->class* (make-hash-table :test #'eql :size 6))

(defmacro defframe (name code slots doc)
  `(let ((class
           (defclass ,name (frame)
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

(defframe continuation #x0
  ()
  "Continuation frame")

(defframe text #x1
  ((data
    :accessor data
    :type string))
  "text frame")

(defframe binary #x2
  ((data
    :accessor data
    :type (array (unsigned-byte 8) (*))))
  "Binary frame")

(defframe close #x8
  ()
  "Close frame")

(defframe ping #x9
  ()
  "Ping frame")

(defframe pong #xA
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


(defclass state ()
  ((websocket


(defclass eof ()
  ()
