(in-package #:portal)

(define-condition portal-condition (error)
  ())

(define-condition upgrade-problem (portal-condition)
  ((key
    :accessor key
    :initarg :key
    :type keyword)))

(define-condition missing-headers (upgrade-problem)
  ((http-request
    :accessor http-request
    :initarg :http-request
    :type fast-http:http-request))
  (:documentation "If headers are missing this is signalled."))

(define-condition read-failure (portal-condition)
  ((key
    :accessor key
    :initarg :key
    :type keyword)
   (stream
    :accessor r-stream
    :initarg :stream
    :type stream))
  (:report (lambda (obj stream)
             (format stream "Failure reading from Stream: ~A. Key: ~A."
                     (r-stream obj)
                     (key stream)))))

(define-condition excess-length (portal-condition)
  ((fun
    :accessor fun
    :initarg :fun)
   (max
    :accessor max-len 
    :initarg :max)))

(define-condition frame-condition (portal-condition)
  ())

(define-condition bad-control-frame (portal-condition)
  ((frame
    :accessor frame
    :initarg :frame)))

(define-condition length-exceeded (bad-control-frame)
  ((length
    :accessor len
    :initarg :length)))

(define-condition unknown-frame-op (frame-condition)
  ((op
    :accessor op
    :initarg :op)))

(define-condition bad-mask (frame-condition)
  ())

(define-condition not-utf8 (frame-condition)
  ())

(define-condition unsupported-frame (frame-condition)
  ((frame
    :accessor frame
    :initarg :frame)))

(define-condition stash-exceeded (frame-condition)
  ((stash
    :accessor stash
    :initarg :stash
    :type stash)))

(define-condition server-condition (portal-condition)
  ())

(define-condition no-known-server (server-condition)
  ((key
    :accessor key
    :initarg :key))
  (:report
   (lambda (obj stream)
     (format stream "No known server by key: ~A." (key obj)))))

(define-condition server-already-exists-by-key (server-condition)
  ((key
    :accessor key
    :initarg :key))
  (:report
   (lambda (obj stream)
     (format stream "A server by that key already exists: ~A." (key obj)))))

(define-condition invalid-status-code (portal-condition)
  ((key
    :accessor key
    :initarg :key))
  (:report (lambda (obj stream)
             (format stream "Invalid status code: ~A" (key obj)))))
