(in-package #:portal)

(define-constant +sec-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=)

(define-constant +http-version+
  "HTTP/1.1"
  :test #'string=)

(define-constant +byte+ #b11111111)

(define-constant +halfbyte+ #b1111)


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
      (logging "~2%Method: ~A~%Major Version: ~A~%Minor Version: ~A~%Resource: ~A~% ~
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
  (with-accessors ((cap cap))
      server        
    (when (<= cap length)
      (error 'length-exceeded
             :length length
             :frame frame))))
                               
(defmethod read-payload (server frame length mask stream)
  (logging "Reading payload of length ~D.~%" length)
  (let ((payload (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length payload)
      (setf (aref payload index)
            (logxor (eread-byte stream :read-payload)
                    (elt mask (mod index 4)))))))

(defgeneric close-frame-p (frame)
  (:method ((c close))
    t)
  (:method (c)
    nil))
             
(defun fragmentedp (websocket)
  (continuation-type websocket))

(defun configure-fragmentation (websocket frame)
  (setf (continuation-type websocket) (class-of frame)))

(defun start-of-fragmentation-p (websocket frame)
  (when (and (not (fragmentedp websocket));already fragmented
             (not (fin frame));end of frame meaning its only 1 data frame.
             (data-frame-p frame));only text or binary can be fragmented. 
    t))

(defun read-ws-byte-one (stream)
  (let* ((first-byte (eread-byte stream :b0))
         (finp (logbitp 7 first-byte))        
         (rsv1p (logbitp 6 first-byte))
         (rsv2p (logbitp 5 first-byte))
         (rsv3p (logbitp 4 first-byte))
         (opcode (logand first-byte +halfbyte+)))
    (macrolet ((e (s) `(when ,s (error 'rsv-bit-set))))
      (e rsv1p) (e rsv2p) (e rsv3p)
      (values first-byte finp rsv1p rsv2p rsv3p opcode))))

(defun read-ws-byte-two (stream)
  (let* ((second-byte (eread-byte stream :b1))
         (maskp (logbitp 7 second-byte))
         (length (logand second-byte #b1111111)))
    ;;we are acting a server. mask must always be set
    (unless maskp
      (error 'mask-not-set))
    (values second-byte maskp length)))
    
(defun read-frame (websocket)
  #.(ds "Read a from from the stream of WEBSOCKET. ~
         When the frame is the last one in a series, return the complete message. ~
         Evals to a frame.")
  (logging "Reading frame.~%")
  (let ((stream (socket-stream websocket)))
  (multiple-value-bind (b0 fin rsv1p rsv2p rsv3p opcode)
      (read-ws-byte-one stream)
    (declare (ignore rsv1p rsv2p rsv3p))
    (multiple-value-bind (b1 maskbit len)
        (read-ws-byte-two stream)
      (let ((frame (make-frame opcode :fin fin)))
        (logging "b0: ~b. b1: ~b. Fin: ~A. Opcode: ~b. ~
                  Maskbit: ~b.~%Len: ~D.~%Frame type: ~A.~%"
                 b0 b1 fin opcode maskbit len (class-of frame))                        
        (setf (opcode websocket) opcode)
        (when (start-of-fragmentation-p websocket frame)          
          ;;if we are not already fragmented and its not finished then we know
          ;;that we are at the 
          (configure-fragmentation websocket frame))      
        ;; read length
        (let* ((len (read-length len stream))
               ;;read mask
               (mask (read-mask stream)))        
          (setf (len frame) len
                (mask frame) mask)
          (logging "Complete frame read: ~S.~%" frame)
          frame))))))

(defun write-length (stream length)
  (logging "Writing length: ~D.~%" length)
  (let* ((2-to-16 65536)
         (val (cond ((<= length 125) length)
                    ((<= length 2-to-16) 126)
                    ((< 2-to-16 length) 127))))
    (write-byte val stream)
    ;; extended length length
    (cond ((< 2-to-16 length)
           (nibbles:write-ub64/be length stream))
          ((<= 125 length)
           ;; extended once
           (nibbles:write-ub16/be length stream)))))

(defun handle-upgrade (server websocket)
  (with-accessors ((socket-stream socket-stream)
                   (header header))
      websocket
    ;; send accept response header
    (force-write (build-header +http-version+ 101
                               "Switching Protocols"
                               :upgrade "websocket"
                               :connection "Upgrade"
                               :sec-websocket-accept
                               (sha1-base64 (str:concat (sec-websocket-key header)
                                                        +sec-key+)))
                 socket-stream)
    ;; custom connect function
    (on-open (path websocket) server websocket)
    (change-class websocket 'ready)))

(defgeneric ready-or-closing-p (websocket)
  (:method ((r ready))
    t)
  (:method ((c closing))
    t)
  (:method (n)
    nil))

(defgeneric read-frame-loop (server websocket)
  (:documentation
   #.(ds "Main loop for reading frames, handling conditions and processing the ~
          received frames. If a condition is signalled in the primary method ~
          then #'handle-condition is evaluated with the condition."))
  (:method :around (server websocket)
    (handler-case (call-next-method)
      (serious-condition (c)
        (logging "Condition handling in #'read-frame-loop: ~A~%" c)
        (handle-condition c server websocket))))
  (:method (server websocket)
    (while ((ready-or-closing-p websocket))
      ;; read new payload
      (logging "Websocket: ~A~%" websocket)
      (let ((frame (read-frame websocket)))
        (handle-frame server frame websocket)))
    websocket))

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
                           (handle-upgrade server websocket)
                           (logging "Upgrade complete. ~%")
                           (read-frame-loop server websocket))
        (portal-condition (c)
          (logging "Condition handling in #'websocket-handler: ~A ~%" c)
          (handle-condition c server websocket))
        (serious-condition (c)
          ;;this is a fallback condition handler.
          (fallback-condition-handler c server websocket))))))         
