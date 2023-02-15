; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/exn
         racket/tcp
         racket/port
         unison/data
         openssl
         )

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

; tests in here
; unison-src/transcripts-using-base/net.md

(define (clientSocket.impl.v3 host port)
    (let-values ([(input output) (tcp-connect host (string->number port))])
        (right (list input output))))

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
    (let ([ports (car tls)]
          [config (cdr tls)])
        (ssl-set-verify! (car ports) #t)
        (right none)))

(define (send.impl.v3 tls data)
    (let* ([ports (car tls)]
           [output (cdr ports)]
           [config (cdr tls)])
        (write-bytes data output)
    (right none)))

(define (receive.impl.v3 tls)
    (right (port->bytes (car (car tls)) #:close? #f)))

(define (terminate.impl.v3 tls)
    (let ([ports (car tls)])
        (ssl-abandon-port (car ports))
        (ssl-abandon-port (cdr ports))
        (right none)))


