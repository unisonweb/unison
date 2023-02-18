; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/exn
         racket/tcp
         racket/port
         unison/data
         openssl)

(provide
    (prefix-out unison-FOp-IO.   (combine-out
        clientSocket.impl.v3))
    (prefix-out unison-FOp-Tls.  (combine-out
        ClientConfig.default
        handshake.impl.v3
        newClient.impl.v3
        receive.impl.v3
        send.impl.v3
        terminate.impl.v3)))

; TODO check out the tests in here
; unison-src/transcripts-using-base/net.md

(define (clientSocket.impl.v3 host port)
    (with-handlers
        [[exn:fail:network? (lambda (e) (exception "IOFailure" (exn->string e) '()))]
         [(lambda _ #t) (lambda (e) (exception "MiscFailure" "Unknown exception" e))] ]

        (let-values ([(input output) (tcp-connect host (string->number port))])
            (right (list input output)))))

(define (ClientConfig.default host certificateSuffix)
    (list host certificateSuffix))

(define (newClient.impl.v3 config socket)
    (let ([i (car socket)]
          [o (car (cdr socket))]
          [hostname (car config)]
          [cert-suffix (car (cdr config))]
    )
    (let-values ([(in out) (ports->ssl-ports
                                i o
                                #:hostname hostname
                                #:close-original? #t
                                )])
        (right (cons (cons in out) config)))))

(define (handshake.impl.v3 tls)
    (ssl-set-verify! (car (car tls)) #t)
    (right none))

(define (send.impl.v3 tls data)
    (let* ([ports (car tls)]
           [output (cdr ports)]
           [config (cdr tls)])
        (write-bytes data output)
        (right none)))

(define (receive.impl.v3 tls)
    (right (port->bytes (car (car tls)) #:close? #f)))

(define (terminate.impl.v3 tls)
    ; NOTE: This actually does more than the unison impl,
    ; which only sends the `close_notify` message, and doesn't
    ; mark the port as no longer usable in the runtime.
    ; Not sure if this is an important difference.
    ; Racket's openssl lib doesn't expose a way to *just* call
    ; SSL_Shutdown on a port without also closing it.
    (let ([ports (car tls)])
        (ssl-abandon-port (car ports))
        (ssl-abandon-port (cdr ports))
        (right none)))


