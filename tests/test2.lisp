(ql:quickload :websocket-driver-client)

(defparameter *client* (wsd:make-client "ws://localhost:5005/echo"))
(defparameter *server* (pws:server 5005))

(pws:server-close *server*)

(pws:define-resource "/echo"
  :open (lambda (websocket)
          (pws:send websocket "Welcome to echo server."))
  :message (lambda (websocket message)
             (logging "Websocket: ~S~%" websocket)
             (sleep 1)
             (pws:send websocket (format nil "~A from client" message))))




(wsd:start-connection *client*)
(wsd:on :message *client*
        (lambda (message)
          (format t "~&Got: ~A~%" message)))
(wsd:send *client* (make-string 127 :initial-element #\h))
(wsd:close-connection *client*)


;;the interface



(defclass my-server (server)
  ()
  (:default-initargs :port 5006
                     :paths '(#P"/echo" #P"/foo")))

(defmethod on-open ((path (eql #P"/echo")) (server my-server) websocket)
  "REE")

