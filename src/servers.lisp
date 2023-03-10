(in-package #:portal)

#||
This file contains the code for working with Servers. This will allow the user to define
multiple listening websockets.
||#

(defvar *servers* (make-hash-table :test #'eq)
  "Key -> server mapping for storing servers.")

(defun new-server (class key port paths &key (origins nil))
  "Make a new server under KEY that is a subclass of 'server. Listens on PORT."
  (restart-case
      (when (gethash key *server*)
        (error 'server-already-exists-by-key
               :key key))
    (continue ()
      :report "Continue?"
      nil)
    (stop-other ()
      :report "Stop server by that name?"
      (stop-server key))
    (quit ()
      :report "Quit?"
      (return-from new-server nil)))
  (setf (gethash key *servers*)
        (make-instance class
                       :origins origins
                       :paths paths
                       :websocket (make-instance 'websocket)
                       :port port)))

(defun get-server (key)
  (or (gethash key *server*)
      (error 'no-known-server :key key)))
                         
(defun server (websocket &key (port 4433) (multi-thread nil))
  (socket-server *wildcard-host* port
                 (lambda (stream)
                   (block :bail
                     (handler-bind
                         ((error
                            (lambda (condition)
                              (format *error-output* "~A~%" condition)
                              (unless *debug-on-error* (return-from :bail)))))
                       (setf (socket-stream websocket) stream)
                       (websocket-handler websocket))))
    nil
    :in-new-thread t
    :multi-threading multi-thread
    :element-type '(unsigned-byte 8)
    :name (format nil "Websocket Server port: ~D" port)))

(defun server-close (websocket-server)
  (when (bt:thread-alive-p websocket-server)
    (logging "Stopping websocket-server.")
    (bt:destroy-thread websocket-server)))

(defun start-server (key &key (multi-thread nil))
  (let ((server (get-server key)))
    (with-accessors ((thread thread)
                     (websocket websocket)
                     (port port))
        server
      (unless (bt:thread-alive-p thread)
        (setf thread
              (server websocket :port port
                                :multi-thread multi-thread))))))

(defun stop-server (key)
  (let ((server (get-server key)))
    (with-accessors ((thread thread))
        server
      (server-close thread))))
      

        
        
    

                       
