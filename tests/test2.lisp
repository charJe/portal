(ql:quickload :websocket-driver-client)

(defparameter *client* (wsd:make-client "ws://localhost:5005/echo"))

(wsd:start-connection *client*)
(wsd:on :message *client*
        (lambda (message)
          (format t "~&Got: ~A~%" message)))
(wsd:send *client* (make-string 9 :initial-element #\k))
(wsd:close-connection *client*);;this doesn't seem to close properly it doesn't send the
;;closing frame... or if it does its closed the stream before its possible to read it.

(defclass my-server (server)
  ()
  (:default-initargs :port 5005
                     :paths '(#P"/echo" #P"/foo")
                     :cap 1000))

(defmethod on-open ((path (eql #P"/echo")) (server my-server) websocket)
  (format *debug-io* "Openin init.~%"))

(defmethod on-message ((path (eql #P"/echo")) (server my-server) websocket message)
  (send websocket (format nil "~A from client" message)))

(defparameter *test-server* (new-server 'my-server :test))
