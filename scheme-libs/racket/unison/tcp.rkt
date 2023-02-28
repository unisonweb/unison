; TLS primitives! Supplied by openssl (libssl)
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
  (close-input-port (input socket))
  (close-output-port (output socket))
  (right none))

(define (clientSocket.impl.v3 host port)
  (with-handlers
      [[exn:fail:network? (lambda (e) (exception "IOFailure" (exn->string e) '()))]
       [exn:fail:contract? (lambda (e) (exception "InvalidArguments" (exn->string e) '()))]
       [(lambda _ #t) (lambda (e) (exception "MiscFailure" "Unknown exception" e))] ]

    (let-values ([(input output) (tcp-connect host (string->number port))])
      (right (list input output)))))

(define (socketSend.impl.v3 socket data)
  (write-bytes data (output socket))
  (flush-output (output socket))
  (right none))

(define (socketReceive.impl.v3 socket amt)
  (let ([buffer (make-bytes amt)])
    (read-bytes-avail! buffer (input socket))
    (right buffer))
  )

(define (socketPort.impl.v3 socket)
  (let-values ([(_ local-port __ ___) (tcp-addresses (input socket) #t)])
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
        (display "hostname ")
        (display hostname)
        (display "\n")
        (let ([listener (tcp-listen (string->number port) 4 #f (if (equal? 0 hostname) #f hostname))])
          (right listener))))))

; NOTE: This is a no-op, because racket's public TCP stack doesn't have separate operations for
; "bind" vs "listen". Because of this, `serverSocket` binds and listens at the same time.
(define (listen.impl.v3 _listener)
  (right none))

(define (socketAccept.impl.v3 listener)
  (let-values ([(input output) (tcp-accept listener)])
    (right (list input output))))
