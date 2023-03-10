(in-package #:portal)

#||
This file contains the code for working with Servers. This will allow the user to define
multiple listening websockets.
||#

(defvar *servers* (make-hash-table :test #'eq)
  "Key -> server mapping for storing servers.")

(defun new-server (class key)
  "Make a new server under KEY that is a subclass of 'server. Listens on PORT."
  (restart-case
      (when (gethash key *servers*)
        (error 'server-already-exists-by-key
               :key key))
    (continue ()
      :report "Continue?"
      nil)
    (stop-other ()
      :report "Stop server by that name?"
      (stop-server key)
      (remhash key *servers*))
    (quit ()
      :report "Quit?"
      (return-from new-server nil)))
  (setf (gethash key *servers*)
        (make-instance class :websocket (make-instance 'websocket))))


(defun get-server (key)
  (or (gethash key *servers*)
      (error 'no-known-server :key key)))
                         
(defun server (server websocket &key (port 4433) (multi-thread nil))
  (socket-server *wildcard-host* port
                 (lambda (stream)
                   (block :bail
                     (handler-bind
                         ((error
                            (lambda (condition)
                              (format *error-output* "~A~%" condition)
                              (unless *debug-on-error* (return-from :bail)))))
                       (setf (socket-stream websocket) stream)
                       (websocket-handler server websocket))))
    nil
    :in-new-thread t
    :multi-threading multi-thread
    :element-type '(unsigned-byte 8)
    :name (format nil "Websocket Server port: ~D" port)))

(defun runningp (server)
  (and (slot-boundp server 'thread)
       (bt:thread-alive-p (slot-value server 'thread))))

(defun start-server (key &key (multi-thread nil))
  (let ((server (get-server key)))
    (with-accessors ((thread thread)
                     (websocket websocket)
                     (port port))
        server
      (unless (runningp server)
        (setf thread
              (server server websocket :port port
                                :multi-thread multi-thread))))))

(defun stop-server (key)
  (let ((server (get-server key)))
    (when (runningp server)
      (bt:destroy-thread (thread server)))))
      

        
        
    

                       
