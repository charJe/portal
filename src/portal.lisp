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
             
(defun read-frame (websocket)
  #.(ds "Read a from from the stream of WEBSOCKET. ~
         When the frame is the last one in a series, return the complete message. ~
         Evals to a frame.")
  (logging "Reading frame.~%")
  (let* ((stream (socket-stream websocket))
         (b0 (eread-byte stream :eof))
         (b1 (eread-byte stream :eof))                
         (fin (< 0 (logand b0 #b10000000)))
         (opcode (logand b0 +halfbyte+))
         (maskbit (or (< 0 (logand b1 #b10000000))
                   (error 'bad-mask)))
         (len (logand b1 #b1111111))
         (frame (make-frame opcode :fin fin)))
    (setf (opcode websocket) opcode)
    (when (start-of-fragmentation-p websocket frame)          
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
      frame)))

(defun write-length (stream length)
  (logging "Writing length: ~D.~%" length)
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
                  (/ (logand length (* +byte+ shift))
                     shift)
                  stream)))
      ;; extended once
       ((<= 125 length)
       (loop :for place :from 1 downto 0
             :for shift = (expt 2 (* 8 place))
             :do (write-byte
                  (/ (logand length (* +byte+ shift))
                     shift)
                  stream))))))

(defun handle-upgrade (server)
  (with-accessors ((websocket websocket))
      server 
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
      (change-class websocket 'ready)
      ;; messages
      (read-frame-loop server)
      (logging "#'read-frame-loop finished.~%"))))
                
(defun handle-cannot-upgrade (stream reason)
  (logging "Cannot upgrade: ~A~%" reason)
  (force-write (build-header +http-version+ 400 "Bad Request"                            
                             :reason reason)
               stream))

(defgeneric ready-or-closing-p (websocket)
  (:method ((r ready))
    t)
  (:method ((c closing))
    t)
  (:method (n)
    nil))

(defgeneric read-frame-loop (server)
  (:documentation
   #.(ds "Main loop for reading frames, handling conditions and processing the ~
          received frames. If a condition is signalled in the primary method ~
          then #'handle-condition is evaluated with the condition."))
  (:method :around (server)
    (handler-case (call-next-method)
      (serious-condition (c)
        (logging "Condition Signalled: ~A~%" c)
        (handle-condition c server (websocket server)))))
  (:method (server)
    (with-accessors ((websocket websocket))
        server      
      (while ((ready-or-closing-p websocket))
        ;; read new payload
        (logging "Websocket: ~A~%" websocket)
        (let ((frame (read-frame websocket)))
          (handle-frame server frame websocket)))
      websocket)))

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
          (handle-cannot-upgrade socket-stream (string-downcase (key c))))
        (serious-condition (c)
          (logging "Fatal condition in #'websocket-handler: ~A~%" c)
          (force-close websocket)
          nil)))))
          
