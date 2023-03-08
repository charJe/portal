(ql:quickload :websocket-driver-client)

(defparameter *client* (wsd:make-client "ws://localhost:5004/echo"))
(defparameter *server* (pws:server 5004))

(pws:server-close *server*)

(pws:define-resource "/echo"
  :open (lambda (websocket)
          (pws:send websocket "Welcome to echo server."))
  :message (lambda (websocket message)
             (sleep 1)
             (pws:send websocket (format nil "~A from client" message))))




(wsd:start-connection *client*)
(wsd:on :message *client*
        (lambda (message)
          (format t "~&Got: ~A~%" message)))
(wsd:send *client* (make-string 127 :initial-element #\h))
(wsd:close-connection *client*)
