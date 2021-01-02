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
(setq global-socket nil)

(pws:define-path-handler "/add1"
  :connect (lambda (socket)
             (pws:send socket "Welcome to add1 server."))
  :message (lambda (socket message)
             (pws:send socket
                   (->> message
                     (parse-integer)
                     (1+)))))

(pws:define-path-handler "/echo"
  :connect (lambda (websocket)
             (pws:send websocket "Welcome to echo server."))
  :message (lambda (websocket message)
             (pws:send websocket message)))

(pws:define-path-handler "/no"
  :connect (lambda (socket)
             (pws:close socket)))

(defparameter server
  (pws:websocket-server 4433 :multi-thread))
