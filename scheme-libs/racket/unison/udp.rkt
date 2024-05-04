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
         (only-in unison/boot sum-case)
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
  ; (prefix-out
  ;   unison-FOp-IO.
  ;   (combine-out
  ;   UDP.clientSocket.impl.v1
  ;   UDP.UDPSocket.recv.impl.v1
  ;   UDP.UDPSocket.send.impl.v1
  ;   UDP.UDPSocket.close.impl.v1
  ;   UDP.ListenSocket.close.impl.v1
  ;   UDP.UDPSocket.toText.impl.v1
  ;   UDP.serverSocket.impl.v1
  ;   UDP.ListenSocket.toText.impl.v1
  ;   UDP.ListenSocket.recvFrom.impl.v1
  ;   UDP.ClientSockAddr.toText.v1
  ;   UDP.ListenSocket.sendTo.impl.v1))))

(struct client-sock-addr (host port))

; Haskell's Network.UDP choice of buffer size is 2048, so mirror that here
(define buffer-size 2048)

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
  (UDPSocket.recv.impl.v1 socket)
  (let 
    ([rv (handle-errors (lambda() 
      (let*-values
        ([(buffer) (make-bytes buffer-size)]
         [(len a b) (udp-receive! socket buffer)])
        (right (bytes->chunked-bytes (subbytes buffer 0 len))))))])
    (sum-case rv
      (0 (err code msg)
        (ref-either-left (ref-failure-failure err code (data builtin-any:typelink 0 msg))))
      (1 (data)
        (ref-either-right data)))))

(define-unison
  (ListenSocket.close.impl.v1 socket)
  (let 
    ([rv (handle-errors (lambda() 
      (udp-close socket)
      ref-unit-unit))])
    (sum-case rv
      (0 (err code msg)
        (ref-either-left (ref-failure-failure err code (data builtin-any:typelink 0 msg))))
      (1 ()
        (ref-either-right ref-unit-unit)))))

(define-unison
  (serverSocket.impl.v1 x0 x1)
    (let* ([x2 (UDP.serverSocket.impl.v1 x0 x1)])
      (sum-case x2
        (0 (x3 x4 x5)
          (let* ([x6 (data builtin-any:typelink 0 x5)]
                 [x7 (data ref-failure:typelink 0 x3 x4 x6)])
            (data ref-either:typelink 1 x7)))
        (1 (x3)
          (data ref-either:typelink 0 x3)))))

(define-unison
  (ListenSocket.recvFrom.impl.v1 socket)
  (if (not (udp? socket))
    (exception ref-iofailure:typelink (string->chunked-string "Not a UDP socket"))
      (let*-values ([(buffer) (make-bytes buffer-size)]
                    [(len host port) (udp-receive! socket buffer)]
                    [(csa) (client-sock-addr host port)]
                    [(tmp-host) (client-sock-addr-host csa)]
                    [(tmp-port) (client-sock-addr-port csa)]
                    [(bs) (subbytes buffer 0 len)]
                    [(chunked) (bytes->chunked-bytes bs)])
        (ref-either-right (ref-tuple-pair chunked (ref-tuple-pair csa ref-unit-unit))))))

(define-unison
  (UDPSocket.send.impl.v1 x0 x1)
  (let* ([x2 (UDP.UDPSocket.send.impl.v1 x0 x1)])
    (sum-case x2
      (0 (x3 x4 x5)
        (let* ([x6 (data builtin-any:typelink 0 x5)]
               [x7 (data ref-failure:typelink 0 x3 x4 x6)])
          (data ref-either:typelink 1 x7)))
        (1 ()
          (let* ([x3 (data ref-unit:typelink 0)])
            (data ref-either:typelink 0 x3))))))

(define-unison
  (ListenSocket.sendTo.impl.v1 sock bytes addr)
  (let* ([res (UDP.ListenSocket.sendTo.impl.v1 sock bytes addr)])
    (sum-case res
      (0 (err code msg)
        (
          (ref-either-left (ref-failure-failure err code (data builtin-any:typelink 0 msg)))))
      (1 ()
        (ref-either-right ref-unit-unit)))))

(define-unison
  (UDPSocket.toText.impl.v1 x0)
  (UDP.UDPSocket.toText.impl.v1 x0))

(define-unison
  (ClientSockAddr.toText.v1 x0)
  (UDP.ClientSockAddr.toText.v1 x0))

(define-unison
  (ListenSocket.toText.impl.v1 x0)
  (UDP.ListenSocket.toText.impl.v1 x0))

(define-unison
  (UDPSocket.close.impl.v1 x0)
  (let* ([x1 (UDP.UDPSocket.close.impl.v1 x0)])
    (sum-case x1
      (0 (x2 x3 x4)
        (let* ([x5 (data builtin-any:typelink 0 x4)]
               [x6 (ref-failure-failure x2 x3 x5)])
          (ref-either-left x6)))
      (1 (x2)
        (ref-either-right x2)))))
    ; (let* ([result (unison-FOp-IO.UDP.clientSocket.impl.v1 host port)])
    ;   (sum-case result
    ;     (0 (x3 x4 x5)
    ;       (let* ([x6 (data builtin-any:typelink 0 x5)]
    ;              [x7 (data ref-failure:typelink 0 x3 x4 x6)])
    ;         (data ref-either:typelink 1 x7)))
    ;     (1 (x3)
    ;       (data ref-either:typelink 0 x3)))))

; (define-unison (clientSocket.impl.v1 host port)
;   compose (wrap-in-either (handle-errors (lambda ()
;     (let* ([pport (string->number (chunked-string->string port))]
;            [hhost (chunked-string->string host)]
;            [sock (udp-open-socket hhost pport)]
;            [_ (udp-bind! sock #f 0)]
;            [res (udp-connect! sock hhost pport)])
;       (right sock))))))

(define-unison
  (clientSocket.impl.v1 host port)
  (let ([rv (handle-errors (lambda() (let* ([pport (string->number (chunked-string->string port))]
         [hhost (chunked-string->string host)]
         [sock (udp-open-socket hhost pport)]
         [_ (udp-bind! sock #f 0)]
         [res (udp-connect! sock hhost pport)]) (right sock))))])
    (sum-case rv
      (0 (err code msg)
        (ref-either-left (ref-failure-failure err code (data builtin-any:typelink 0 msg))))
      (1 (sock)
        (ref-either-right sock)))))

; (define (UDP.clientSocket.impl.v1 host port) ; string string -> socket
;   (handle-errors
;    (lambda ()
;     (begin
;      (let* ([pport (string->number (chunked-string->string port))]
;             [hhost (chunked-string->string host)]
;             [sock (udp-open-socket hhost pport)]
;             [_ (udp-bind! sock #f 0)]
;             [res (udp-connect! sock hhost pport)])
;        (right sock))))))

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


; (define (UDP.UDPSocket.recv.impl.v1 socket) ; socket -> bytes
;   (handle-errors
;     (lambda ()
;       (begin
;         (let*-values (
;                       [(buffer) (make-bytes buffer-size)]
;                       [(len a b) (udp-receive! socket buffer)])
;           (right (bytes->chunked-bytes (subbytes buffer 0 len))))))))


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
      (begin
        (udp-close socket)
        (right none)))))

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
; (define (UDP.ListenSocket.sendTo.impl.v1 socket data addr) ; socket bytes clientsockaddr -> ()
;   (handle-errors
;     (lambda ()
;       (begin
;         (udp-send socket (chunked-bytes->bytes data) (client-sock-addr-host addr) (client-sock-addr-port addr))
;         (right none)))))
(define
  (UDP.ListenSocket.sendTo.impl.v1 socket data addr)
    (let* ([host (client-sock-addr-host addr)]
           [port (client-sock-addr-port addr)]
           [bytes (chunked-bytes->bytes data)])
      (begin
        (udp-send-to socket host port bytes)
        (right none))))