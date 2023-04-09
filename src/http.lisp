(in-package #:portal)

(defun %new-ba ()
  (make-array 256 :element-type '(unsigned-byte 8)))

(define-constant +cr+ 13
  :test #'=)

(define-constant +lf+ 10
  :test #'=)

(defun read-headers (stream)
  (let ((arrays ())
        (ba (%new-ba))
        (pos 0)
        (last-val 0)
        (crlf-count 0))
    (flet ((add-val (byte)
             (setf last-val byte)
             (if (= pos 256)
                 (progn (push ba arrays);reset
                        (setf ba (%new-ba)
                              pos 1
                              (aref ba 0) byte))
                 (progn (setf (aref ba pos) byte)
                        (incf pos))))
           (double-cr (byte)
             (let ((crlf? (and (= last-val +cr+)
                               (= byte +lf+))))
               (if crlf?
                   (progn (incf crlf-count)
                          (when (= crlf-count 2)
                            t))
                   (progn (unless (= byte +cr+)
                            (setf crlf-count 0))
                          nil)))))
      (do ((byte (eread-byte stream :headers)
                 (eread-byte stream :headers)))
          ((double-cr byte)
           (add-val byte);;add the final linefeed
           (parse-headers
            (correct-headers
             (if arrays
                 (progn (push ba arrays)
                        arrays)
                 ba)
             pos)))
        (add-val byte)))))

(defun correct-headers (headers final-pos)
  (typecase headers
    (array (subseq headers 0 final-pos))
    (list (progn (setf (first headers) (subseq (first headers) 0 final-pos))
                 (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                        (nreverse headers))))))

(defun parse-headers (headers)
  (let* ((http (fast-http:make-http-request))
         (parser (fast-http:make-parser http)))                                               
    (funcall parser headers)))

(defun http-headers (http)
  (let ((h (fast-http:http-headers http)))
    (unless (hash-table-p h)
      (error 'missing-headers :http-request http))
    h))

(defun upgrade (http)
  (gethash "upgrade" (http-headers http)))

(defun connection (http)
  (gethash "connection" (http-headers http)))

(defun sec-websocket-key (http)
  (gethash "sec-websocket-key" (http-headers http)))

(defun sec-websocket-version (http)
  (gethash "sec-websocket-version" (http-headers http)))

(defun host (http)
  (gethash "host" (http-headers http)))

(defun origin (http)
  (gethash "origin" (http-headers http)))

(defun resource (http)
  (pathname (fast-http:http-resource http)))





