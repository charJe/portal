(in-package #:portal)

#||
This file contains helpers.

||#

(alexandria:define-constant +crlf+
    (format nil "~C~C" #\Return #\Newline)
  :test #'string=)

(defmacro ds (control-string &rest arguments)
  `(format nil ,control-string ,@arguments))

(defun build-header (version code code-meaning &rest headers &key &allow-other-keys)
  ;;compiler macro perfectly possible here. Just change the headers to their :| .. counter
  ;;in order to avoid using ~:(..~) to upcase start of words
  (string-to-octets
   (with-output-to-string (stream)
     (format stream "~A ~A ~A~A" version code code-meaning +crlf+)
     (alexandria:doplist (header header-val headers)
       (format stream "~:(~A~): ~A~A"
               (string header)
               header-val
               +crlf+))
     (format stream "~A" +crlf+))))

(defun sha1-base64 (string)
  (let ((sha1 (ironclad:make-digest 'ironclad:sha1))
        (bin-data (ironclad:ascii-string-to-byte-array string)))
    (ironclad:update-digest sha1 bin-data)
    (cl-base64:usb8-array-to-base64-string
     (ironclad:produce-digest sha1))))

(defun octets-to-string (octets)
  (babel:octets-to-string octets))

(defun string-to-octets (string)
  (babel:string-to-octets string))

(defparameter *noval* :noval)

(defun eread-byte (stream key)
  (let ((val (read-byte stream nil *noval*)))
    (when (eq val *noval*)
      (error 'read-failure
             :key key
             :stream stream))
    val))
  
(defparameter *log* t)

(defmacro logging (control-string &rest arguments)
  `(when *log*
     (format *debug-io* ,control-string ,@arguments)))

(defun force-write (sequence stream &key keys &allow-other-keys)
  (apply #'write-sequence sequence stream keys)
  (force-output stream))

(defmacro while ((condition) &body body)
  `(loop :while ,condition
         :do (locally ,@body)))
  
