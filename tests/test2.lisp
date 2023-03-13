(ql:quickload :websocket-driver-client)

(defparameter *client* (wsd:make-client "ws://localhost:5007/echo"))

(wsd:start-connection *client*)
(wsd:on :message *client*
        (lambda (message)
          (format t "~&Got: ~A~%" message)))
(wsd:send *client* (make-array 1001 :element-type '(unsigned-byte 8) :initial-element 1))
(wsd:close-connection *client*);;this doesn't seem to close properly it doesn't send the
;;closing frame... or if it does its closed the stream before its possible to read it.


(defclass my-server (server)
  ()
  (:default-initargs :port 5007
                     :paths '(#P"/echo" #P"/foo")
                     :stash-cap 1000))

(defmethod on-open ((path (eql #P"/echo")) (server my-server) websocket)
  (format *debug-io* "Openin init.~%"))

(defmethod on-message ((path (eql #P"/echo")) (server my-server) websocket message)
  (send websocket (format nil "~A from client" message))
  (close server websocket))

(defparameter *test-server* (new-server 'my-server :test2))


