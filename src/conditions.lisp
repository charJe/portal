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

(define-condition no-defined-resource (portal-condition)
  ((resource
    :accessor resource
    :initarg :resource))
  (:documentation "Signalled when there is no resource when one is expected."))

(define-condition read-failure (portal-condition)
  ((key
    :accessor key
    :initarg :key
    :type keyword)))

(define-condition excess-length (portal-condition)
  ((fun
    :accessor fun
    :initarg :fun)
   (max
    :accessor max-len 
    :initarg :max)))
    
    

   
(define-condition frame-condition (portal-condition)
  ())

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

  


