; UDP primitives!
#lang racket/base
(require racket/exn
         racket/match
         racket/udp
         unison/data
         unison/data-info
         unison/chunked-seq
         unison/core)

(provide
 (prefix-out
  unison-FOp-IO.
  (combine-out
   UDP.clientSocket.impl.v1
   UDP.UDPSocket.recv.impl.v1
   UDP.UDPSocket.send.impl.v1
  ;  UDP.UDPSocket.socket.impl.v1
   UDP.UDPSocket.close.impl.v1
   UDP.ListenSocket.close.impl.v1
   UDP.UDPSocket.toText.impl.v1
   UDP.serverSocket.impl.v1
   UDP.ListenSocket.toText.impl.v1
   UDP.ListenSocket.recvFrom.impl.v1
   UDP.ClientSockAddr.toText.v1
   ;  UDP.ListenSocket.socket.impl.v1
   UDP.ListenSocket.sendTo.impl.v1)))

(struct client-sock-addr (host port))

; Haskell's Network.UDP choice of buffer size is 2048, so mirror that here
(define buffer-size 2048)

(define (handle-errors fn)
  (with-handlers
      [[exn:fail:network?
         (lambda (e)
           (exception
             ref-iofailure:typelink
             (exception->string e)
             ref-unit-unit))]
       [exn:fail:contract?
         (lambda (e)
           (exception
             ref-miscfailure:typelink
             (exception->string e)
             ref-unit-unit))]
       [(lambda _ #t)
        (lambda (e)
          (exception
            ref-miscfailure:typelink
            (chunked-string->string
              (format "Unknown exception ~a" (exn->string e)))
            ref-unit-unit))]]
    (fn)))


(define (UDP.clientSocket.impl.v1 host port) ; string string -> socket
  (handle-errors
   (lambda ()
    (begin
     (let* ([pport (string->number (chunked-string->string port))]
            [hhost (chunked-string->string host)]
            [sock (udp-open-socket hhost pport)]
            [res (udp-connect! sock hhost pport)])
       (right sock))))))


(define (UDP.UDPSocket.recv.impl.v1 socket) ; socket -> bytes
  (handle-errors
    (lambda ()
      (begin
        (let* ([buffer (make-bytes buffer-size)]
              [_ (udp-receive! socket buffer)])
          (right (bytes->chunked-bytes (subbytes buffer 0 read))))))))


(define (UDP.UDPSocket.send.impl.v1 socket data) ; socket -> bytes -> ()
  (handle-errors
    (lambda ()
      (begin
        (udp-send socket (chunked-bytes->bytes data))
        (right none)))))

;    UDP.UDPSocket.close.impl.v1
(define (UDP.UDPSocket.close.impl.v1 socket)
  (handle-errors
    (lambda ()
      (udp-close socket)
      ref-unit-unit)))

;    UDP.ListenSocket.close.impl.v1
(define (UDP.ListenSocket.close.impl.v1 socket)
  (handle-errors
    (lambda ()
      (udp-close socket)
      ref-unit-unit)))

;    UDP.UDPSocket.toText.impl.v1
(define (UDP.UDPSocket.toText.impl.v1 _) ; socket -> string
  (string->chunked-string "<socket>"))

;    UDP.serverSocket.impl.v1
(define (UDP.serverSocket.impl.v1 ip port) ; string string -> socket
  (handle-errors
   (lambda ()
     (define iip (chunked-string->string ip))
      (define pport (string->number (chunked-string->string port)))
      (begin
        (let* ([sock (udp-open-socket iip pport)]
              [bound-sock (udp-bind! sock iip pport)])
          (right bound-sock))))))
  
;    UDP.ListenSocket.toText.impl.v1
(define (UDP.ListenSocket.toText.impl.v1 _) ; socket -> string
  (string->chunked-string "<socket>"))

;    UDP.ListenSocket.recvFrom.impl.v1
(define (UDP.ListenSocket.recvFrom.impl.v1 socket) ; socket -> (bytes, clientsockaddr)
  (handle-errors
    (lambda ()
      (begin
        (let* ([buffer (make-bytes buffer-size)]
               [result (udp-receive! socket buffer)])
          (match result
            [ (list _ host port)
              (list (bytes->chunked-bytes (subbytes buffer 0 read))
                    (client-sock-addr host port))]))))))

;    UDP.ClientSockAddr.toText.v1
(define (UDP.ClientSockAddr.toText.v1 addr) ; clientsockaddr -> string
  (string->chunked-string (format "<client-sock-addr ~a ~a>" (client-sock-addr-host addr) (client-sock-addr-port addr))))

;    UDP.ListenSocket.sendTo.impl.v1
(define (UDP.ListenSocket.sendTo.impl.v1 socket data addr) ; socket bytes clientsockaddr -> ()
  (handle-errors
    (lambda ()
      (begin
        (udp-send socket (chunked-bytes->bytes data) (client-sock-addr-host addr) (client-sock-addr-port addr))
        (right none)))))

;    UDP.ListenSocket.socket.impl.v1
