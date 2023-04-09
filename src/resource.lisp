(in-package #:portal)

#||
This file contains the code for mimicking the old resource callbacks that were used to
provide functionality for the websocket.
||#

(defgeneric on-open (path server websocket)
  (:argument-precedence-order server path websocket)
  (:documentation
   #.(ds "This generic function is specialized in order to provide functionality when ~
          a connection is established between SERVER and a client. ~
          Specialize the PATH using an EQL #P<mypath> in order to have the correct ~
          method called and subclass server. ~
          Precedence is: SERVER PATH WEBSOCKET. ~
          Does nothing by default."))
  (:method (path server websocket)
    nil))

(defgeneric on-message (path server websocket message)
  (:argument-precedence-order server path message websocket)
  (:documentation
   #.(ds "This generic function is specialized in order to provide functionality when ~
          a connection sends a message to the server. ~
          Each fragment is sent individually, there is no storage for the messages. ~
          You can specialize on MESSAGE to change how you handle binary/text data. ~
          Specialize the PATH using an EQL #P<mypath> in order to have the correct ~
          method called and subclass server. ~
          Precedence is: SERVER PATH MESSAGE WEBSOCKET.
          Has to be specialized.")))

(defgeneric on-close (path server websocket)
  (:argument-precedence-order server path websocket)
  (:documentation
   #.(ds "This generic function is specialized in order to provide functionality when ~
          a connection is closed. ~
          Specialize the PATH using an EQL #P<mypath> in order to have the correct ~
          method called and subclass server. ~
          Precedence is: SERVER PATH WEBSOCKET. ~
          Does nothing by default."))
  (:method (path server websocket)
    nil))

(defgeneric on-condition (path server websocket condition)
  (:argument-precedence-order server path condition websocket)
  (:documentation
   #.(ds "This generic function is specialized in order to provide functionality when ~
          a connection is closed. ~
          Specialize the PATH using an EQL #P<mypath> in order to have the correct ~
          method called and subclass server. ~
          Precedence is: SERVER PATH CONDITION WEBSOCKET. ~
          Does nothing by default."))
  (:method (path server websocket condition)
    nil))
