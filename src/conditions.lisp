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
   
   
