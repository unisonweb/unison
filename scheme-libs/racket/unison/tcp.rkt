; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/exn
         racket/tcp
         unison/data)

(provide (prefix-out unison-FOp-IO.   (combine-out
        clientSocket.impl.v3
        closeSocket.impl.v3
        socketReceive.impl.v3
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
    (right (read-bytes amt (input socket))))