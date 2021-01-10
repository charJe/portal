(ql:quickload 'portal)
;; (assert (equal (header->alist
;;                 (str:join +crlf+ '("GET /chat?name=value HTTP/1.1"
;;                                    "Host: example.com:8000"
;;                                    "Upgrade: websocket"
;;                                    "Connection: Upgrade"
;;                                    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="
;;                                    "Sec-WebSocket-Version: 13" "")))
;;                '((:METHOD . "GET") (:SCRIPT . "/chat") (:QUERY-STRING . "name=value")(:VERSION . "HTTP/1.1")
;;                  (:HOST . "example.com:8000") (:UPGRADE . "websocket")
;;                  (:CONNECTION . "Upgrade") (:SEC-WEBSOCKET-KEY . "dGhlIHNhbXBsZSBub25jZQ==")
;;                  (:SEC-WEBSOCKET-VERSION . "13"))))

;; (assert (string= (alist->header
;;                   '((:version . "HTTP/1.1")
;;                     (:code . "101")
;;                     (:code-meaning . "Switching Protocols")
;;                     (:upgrade . "websocket")
;;                     (:connection . "Upgrade")
;;                     (:sec-websocket-accept . "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")))
;;                  (concatenate 'string (str:join +crlf+
;;                                                 '("HTTP/1.1 101 Switching Protocols"
;;                                                   "UPGRADE: websocket"
;;                                                   "CONNECTION: Upgrade"
;;                                                   "SEC-WEBSOCKET-ACCEPT: s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))
;;                               +crlf+ +crlf+)))

(use-package 'arrows)
(defparameter global-socket nil)

(pws:define-path-handler "/add1"
  :connect (lambda (socket)
             (pws:send socket "Welcome to add1 server."))
  :message (lambda (socket message)
             (pws:send socket
                   (-> message
                     (parse-integer :junk-allowed t)
                     (1+)))))

(defun echo (websocket message)
  (setq global-socket websocket)
  (sleep 1)
  (pws:send websocket message)
  (sleep 1)
  (pws:send websocket message))

(pws:define-path-handler "/echo"
  :connect (lambda (websocket)
             (pws:send websocket "Welcome to echo server."))
  :message #'echo
  :disconnect (lambda (websocket)
                (declare (ignore websocket))
                (print 'leaving-echo)
                (force-output)))

(pws:define-path-handler "/no"
  :connect (lambda (socket)
             (pws:close socket)))

(defparameter server
  (pws:server 4433))
