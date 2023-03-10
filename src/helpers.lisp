(in-package #:portal)

#||
This file contains helpers.

||#

(define-constant +crlf+
    (coerce (list (code-char 13) (code-char 10)) 'string)
  :test #'string=)

(defmacro ds (control-string &rest arguments)
  `(format nil ,control-string ,@arguments))

(defun alist->header (alist)
  "Return the string response header corresponding to ALIST."
  ;;to be replacec
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
      (error 'read-failure :key key))
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
  
