# Portal 2

This is a mostly complete implementation of the websocket server functionality described in
[RFC6455](https://datatracker.ietf.org/doc/html/rfc6455)


- No http servers so it is light weight.
- No http server so no advanced websocket handshakes.
- Doesn't support any extensions, doesn't even check.
- Supports fragmentation by default through #'on-message.
- Doesn't store any message, again use #'on-message.
- Only supports ws not wss, so use a webserver like Nginx in a reverse proxy if you want ssl.


## Functionality

I have completely reworked the interface.

To create an echo server you need similar to below: 

```lisp
(defclass my-server (server)
  ()
  (:default-initargs :port 5005
                     :paths '(#P"/echo")
                     :cap 1000))

(defmethod on-message ((path (eql #P"/echo")) (server my-server) websocket message)
  (send websocket (format nil "~A from client" message)))

(defparameter *test-server* (new-server 'my-server :test))

```

Then

```lisp
(start-server :test)
```

Make sure you specialize the Generic Function #'on-message otherwise this will signal a condition of type no-applicable-method and the connection will be closed.

The other generic functions can be found in `/src/resource.lisp`.

### Initargs to instance of server

There are a few customizable initargs to servers.

- port -- The listening port.
- paths -- A list of pathnames (they have to be pathnames), this is where the server will listen. Allows you to specialize functionality for different URLs but the same server.
- origins -- A list of origins matched against the http request made for the handshake.
- cap -- A size in bytes used to cap the size of fragmented messages to stop OOM.


### User functions

You can
- `#'send`
- `#'send-ping`
- `#'send-pong`
- `#'close` 

You cannot send in a fragmented manner.

Will
- Attempt to finish reading fragmented messages after you send a #'close.

#### Valid status codes for #'close

| As keyword                     | as number |
| --- | --- |
| :NORMAL                        | 1000 |
| :GOING-AWAY                    | 1001 |
| :PROTOCOL-ERROR                | 1002 |
| :INVALID-TYPE                  | 1003 |
| :INCONSISTENT-TYPE             | 1007 |
| :POLICY-VIOLATION              | 1008 |
| :OVERSIZE                      | 1009 |
| :EXTENSION-NEGOTIATION-FAILURE | 1010 |
| :FATAL                         | 1011 |




