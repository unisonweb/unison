; UDP primitives!
#lang racket/base
(require racket/udp
         racket/format
         (only-in unison/boot define-unison)
         unison/data
         unison/data-info
         unison/chunked-seq
         (only-in unison/boot sum-case)
         unison/network-utils
         unison/core)

(provide
  (prefix-out
    builtin-IO.UDP.
    (combine-out
      clientSocket.impl.v1
      clientSocket.impl.v1:termlink
      UDPSocket.recv.impl.v1
      UDPSocket.recv.impl.v1:termlink
      UDPSocket.send.impl.v1
      UDPSocket.send.impl.v1:termlink
      UDPSocket.close.impl.v1
      UDPSocket.close.impl.v1:termlink
      ListenSocket.close.impl.v1
      ListenSocket.close.impl.v1:termlink
      UDPSocket.toText.impl.v1
      UDPSocket.toText.impl.v1:termlink
      serverSocket.impl.v1
      serverSocket.impl.v1:termlink
      ListenSocket.toText.impl.v1
      ListenSocket.toText.impl.v1:termlink
      ListenSocket.recvFrom.impl.v1
      ListenSocket.recvFrom.impl.v1:termlink
      ClientSockAddr.toText.v1
      ClientSockAddr.toText.v1:termlink
      ListenSocket.sendTo.impl.v1
      ListenSocket.sendTo.impl.v1:termlink)))
 

(struct client-sock-addr (host port))

; Haskell's Network.UDP choice of buffer size is 2048, so mirror that here
(define buffer-size 2048)

(define ; a -> Either Failure a
  (wrap-in-either a)
  (sum-case a
    (0 (type msg meta)
      (ref-either-left (ref-failure-failure type msg (unison-any-any meta))))
    (1 (data) 
      (ref-either-right data))))

(define 
  (format-socket socket)
  (let*-values ([(local-hn local-port remote-hn remote-port) (udp-addresses socket #t)]
                [(rv) (~a "<socket local=" local-hn ":" local-port " remote=" remote-hn ":" remote-port ">")])
    (string->chunked-string rv)))

(define (close-socket socket)
  (let ([rv (handle-errors (lambda() (begin
    (udp-close socket)
    (right ref-unit-unit))))])
    (wrap-in-either rv)))

;; define termlink builtins
(define clientSocket.impl.v1:termlink 
  (unison-termlink-builtin "IO.UDP.clientSocket.impl.v1"))
(define UDPSocket.recv.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.UDPSocket.recv.impl.v1"))
(define UDPSocket.send.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.UDPSocket.send.impl.v1"))
(define UDPSocket.close.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.UDPSocket.close.impl.v1"))
(define ListenSocket.close.impl.v1:termlink 
  (unison-termlink-builtin "IO.UDP.ListenSocket.close.impl.v1"))
(define UDPSocket.toText.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.UDPSocket.toText.impl.v1"))
(define serverSocket.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.serverSocket.impl.v1"))
(define ListenSocket.toText.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.ListenSocket.toText.impl.v1"))
(define ListenSocket.recvFrom.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.ListenSocket.recvFrom.impl.v1"))
(define ClientSockAddr.toText.v1:termlink
  (unison-termlink-builtin "IO.UDP.ClientSockAddr.toText.v1"))
(define ListenSocket.sendTo.impl.v1:termlink
  (unison-termlink-builtin "IO.UDP.ListenSocket.sendTo.impl.v1"))

;; define builtins

(define-unison
  (UDPSocket.recv.impl.v1 socket) ; socket -> Either Failure Bytes
  (let 
    ([rv (handle-errors (lambda() 
      (let*-values
        ([(buffer) (make-bytes buffer-size)]
         [(len a b) (udp-receive! socket buffer)])
        (right (bytes->chunked-bytes (subbytes buffer 0 len))))))])
    (wrap-in-either rv)))

(define-unison
  (ListenSocket.close.impl.v1 socket) ; socket -> Either Failure ()
  (close-socket socket))

(define-unison
  (serverSocket.impl.v1 ip port) ; string string -> Either Failure socket
    (let
      ([result (handle-errors (lambda()
        (let* ([iip (chunked-string->string ip)]
              [pport (string->number (chunked-string->string port))]
              [sock (udp-open-socket iip pport)])
          (begin
            (udp-bind! sock iip pport)
            (right sock)))))])
      (wrap-in-either result)))

(define-unison
  (ListenSocket.recvFrom.impl.v1 socket) ; socket -> Either Failure (Bytes, ClientSockAddr)
  (let ([result (handle-errors (lambda() 
    (if (not (udp? socket))
      (raise-argument-error 'socket "a UDP socket" socket)
      (let*-values 
        ([(buffer) (make-bytes buffer-size)]
         [(len host port) (udp-receive! socket buffer)]
         [(csa) (client-sock-addr host port)]
         [(bs) (subbytes buffer 0 len)]
         [(chunked) (bytes->chunked-bytes bs)])
        (right (ref-tuple-pair chunked (ref-tuple-pair csa ref-unit-unit)))))))])
    (wrap-in-either result)))

(define-unison
  (UDPSocket.send.impl.v1 socket data) ; socket -> Bytes -> Either Failure ()
  (let
    ([result (handle-errors (lambda () (begin
      (udp-send socket (chunked-bytes->bytes data)) 
      (right ref-unit-unit))))])
    (wrap-in-either result)))

(define-unison
  (ListenSocket.sendTo.impl.v1 sock bytes addr) ; socket -> Bytes -> ClientSockAddr -> Either Failure ()
  (let
    ([result (handle-errors (lambda() 
      (let* ([host (client-sock-addr-host addr)]
             [port (client-sock-addr-port addr)]
             [bytes (chunked-bytes->bytes bytes)])
        (begin
          (udp-send-to sock host port bytes)
          (right ref-unit-unit)))))])
    (wrap-in-either result)))

(define-unison
  (UDPSocket.toText.impl.v1 socket) ; socket -> string
  (format-socket socket))

(define-unison
  (ClientSockAddr.toText.v1 addr) ; ClientSocketAddr -> string
  (string->chunked-string (format "<client-sock-addr ~a ~a>" (client-sock-addr-host addr) (client-sock-addr-port addr))))

(define-unison
  (ListenSocket.toText.impl.v1 socket) ; socket -> string
  (format-socket socket))

(define-unison
  (UDPSocket.close.impl.v1 socket) ; socket -> Either Failure ()
  (let
    ([rv (handle-errors (lambda() (begin
      (udp-close socket)
      (right ref-unit-unit))))])
    (wrap-in-either rv)))

(define-unison
  (clientSocket.impl.v1 host port) ; string string -> Either Failure socket
  (let ([rv (handle-errors (lambda() (let* ([pport (string->number (chunked-string->string port))]
         [hhost (chunked-string->string host)]
         [sock (udp-open-socket hhost pport)]
         [_ (udp-bind! sock #f 0)]
         [res (udp-connect! sock hhost pport)]) (right sock))))])
    (wrap-in-either rv)))