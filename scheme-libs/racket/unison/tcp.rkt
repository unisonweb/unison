; TCP primitives!
#lang racket/base
(require racket/exn
         racket/match
         racket/tcp
         unison/data)

(provide
 (prefix-out
  unison-FOp-IO.
  (combine-out
   clientSocket.impl.v3
   closeSocket.impl.v3
   socketReceive.impl.v3
   socketPort.impl.v3
   serverSocket.impl.v3
   listen.impl.v3
   socketAccept.impl.v3
   socketSend.impl.v3)))

(define (input socket) (car socket))
(define (output socket) (car (cdr socket)))

(define (closeSocket.impl.v3 socket)
  (if (pair? socket)
      (begin
        (close-input-port (input socket))
        (close-output-port (output socket)))
      (tcp-close socket))
  (right none))

(define (clientSocket.impl.v3 host port)
  (with-handlers
      [[exn:fail:network? (lambda (e) (exception "IOFailure" (exn->string e) '()))]
       [exn:fail:contract? (lambda (e) (exception "InvalidArguments" (exn->string e) '()))]
       [(lambda _ #t) (lambda (e) (exception "MiscFailure" "Unknown exception" e))] ]

    (let-values ([(input output) (tcp-connect host (string->number port))])
      (right (list input output)))))

(define (socketSend.impl.v3 socket data)
  (if (not (pair? socket))
      (exception "InvalidArguments" "Cannot send on a server socket")
      (begin
        (write-bytes data (output socket))
        (flush-output (output socket))
        (right none))))

(define (socketReceive.impl.v3 socket amt)
  (if (not (pair? socket))
      (exception "InvalidArguments" "Cannot receive on a server socket")
      (begin
        (let ([buffer (make-bytes amt)])
          (read-bytes-avail! buffer (input socket))
          (right buffer)))))

; A "connected" socket is represented as a list of (list input-port output-port),
; while a "listening" socket is just the tcp-listener itself.
(define (socketPort.impl.v3 socket)
  (let-values ([(_ local-port __ ___) (tcp-addresses (if (pair? socket) (input socket) socket) #t)])
    (right local-port)))

(define serverSocket.impl.v3
  (lambda args
    (let-values ([(hostname port)
                  (match args
                    [(list _ port) (values #f port)]
                    [(list _ hostname port) (values hostname port)])])

      (with-handlers
          [[exn:fail:network? (lambda (e) (exception "IOFailure" (exn->string e) '()))]
           [exn:fail:contract? (lambda (e) (exception "InvalidArguments" (exn->string e) '()))]
           [(lambda _ #t) (lambda (e) (exception "MiscFailure" "Unknown exception" e))] ]
        (let ([listener (tcp-listen (string->number port) 4 #f (if (equal? 0 hostname) #f hostname))])
          (right listener))))))

; NOTE: This is a no-op because racket's public TCP stack doesn't have separate operations for
; "bind" vs "listen". We've decided to have `serverSocket` do the "bind & listen", and have
; this do nothing.
; If we want ~a little better parity with the haskell implementation, we might set a flag or
; something on the listener, and error if you try to `accept` on a server socket that you haven't
; called `listen` on yet.
(define (listen.impl.v3 _listener)
  (right none))

(define (socketAccept.impl.v3 listener)
  (if (pair? listener)
      (exception "InvalidArguments" "Cannot accept on a non-server socket")
      (begin
        (let-values ([(input output) (tcp-accept listener)])
          (right (list input output))))))
