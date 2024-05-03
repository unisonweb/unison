; UDP primitives!
#lang racket/base
(require racket/exn
         racket/match
         racket/udp
         racket/format
         (only-in unison/boot define-unison identity)
         unison/data
         unison/data-info
         unison/chunked-seq
         unison/core)

(provide
 (combine-out
  client-sock-addr
  client-sock-addr-host
  client-sock-addr-port
  buffer-size
  (prefix-out
    unison-FOp-IO.
    (combine-out
    UDP.clientSocket.impl.v1
    UDP.UDPSocket.recv.impl.v1
    UDP.UDPSocket.send.impl.v1
    UDP.UDPSocket.close.impl.v1
    UDP.ListenSocket.close.impl.v1
    UDP.UDPSocket.toText.impl.v1
    UDP.serverSocket.impl.v1
    UDP.ListenSocket.toText.impl.v1
    UDP.ListenSocket.recvFrom.impl.v1
    UDP.ClientSockAddr.toText.v1
    UDP.ListenSocket.sendTo.impl.v1))))

(struct client-sock-addr (host port))

;;; define the termlink builtins ;;;
; (define builtin-IO.UDP.ListenSocket.recvFrom.impl.v1:termlink
  ; (unison-termlink-builtin "IO.UDP.ListenSocket.recvFrom.impl.v1"))

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
            (string->chunked-string
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
            [_ (udp-bind! sock #f 0)]
            [res (udp-connect! sock hhost pport)])
       (right sock))))))


(define (UDP.UDPSocket.recv.impl.v1 socket) ; socket -> bytes
  (handle-errors
    (lambda ()
      (begin
        (let*-values (
                      [(buffer) (make-bytes buffer-size)]
                      [(len a b) (udp-receive! socket buffer)])
          (right (bytes->chunked-bytes (subbytes buffer 0 len))))))))


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
(define (UDP.UDPSocket.toText.impl.v1 socket) ; socket -> string
  (format-socket socket))

; (define-unison (builtin-IO.UDP.UDPSocket.toText.impl.v1 socket)
;   (if (not (udp? socket))
;     (exception ref-iofailure:typelink (string->chunked-string "Not a UDP socket"))
;     (let*-values ([(local-hn local-port remote-hn remote-port) (udp-addresses socket)]
;                   [(rv) (~a "<socket local=" local-hn ":" local-port " remote=" remote-hn ":" remote-port ">")])
;       (string->chunked-string rv))

;    UDP.serverSocket.impl.v1
(define (UDP.serverSocket.impl.v1 ip port) ; string string -> socket
  (handle-errors
   (lambda ()
     (define iip (chunked-string->string ip))
      (define pport (string->number (chunked-string->string port)))
      (begin
        (let* ([sock (udp-open-socket iip pport)]
              [_ (udp-bind! sock iip pport)])
          (right sock))))))

(define 
  (format-socket socket)
  (let*-values ([(local-hn local-port remote-hn remote-port) (udp-addresses socket #t)]
                [(rv) (~a "<socket local=" local-hn ":" local-port " remote=" remote-hn ":" remote-port ">")])
    (string->chunked-string rv)))

;    UDP.ListenSocket.toText.impl.v1
(define (UDP.ListenSocket.toText.impl.v1 socket) ; socket -> string
  (format-socket socket))

;    UDP.ListenSocket.recvFrom.impl.v1
(define (UDP.ListenSocket.recvFrom.impl.v1 socket) ; socket -> (bytes, clientsockaddr)  
  (handle-errors
    (lambda ()
      (if (not (udp? socket))
        (exception
          ref-iofailure:typelink
          (string->chunked-string "Not a UDP socket")
          ref-unit-unit)
        (begin
          (let*-values ([(buffer) (make-bytes buffer-size)]
                        [(_ host port) (udp-receive! socket buffer)])
            ((bytes->chunked-bytes (subbytes buffer 0 buffer-size))
                  (client-sock-addr host port))
          ))))))

(define-unison
  (builtin-IO.UDP.ListenSocket.recvFrom.impl.v1 socket)
  (if (not (udp? socket))
    (exception ref-iofailure:typelink (string->chunked-string "Not a UDP socket"))
      (let*-values ([(buffer) (make-bytes buffer-size)]
                    [(len host port) (udp-receive! socket buffer)]
                    [(csa) (client-sock-addr host port)]
                    [(bs) (subbytes buffer 0 len)]
                    [(chunked) (bytes->chunked-bytes bs)]
                    [(nullterm) (data ref-unit:typelink 0)]
                    [(csa-null) (data ref-tuple:typelink 0 csa nullterm)]
                    [(dat) (identity chunked)]
                    [(tup) (data ref-tuple:typelink 0 dat csa-null)]
                    [(rv) (data ref-either:typelink 0 tup)])
        rv)))

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
