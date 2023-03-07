; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/port
         racket/exn
         racket/file
         unison/data
         openssl)

(provide
 (prefix-out
  unison-FOp-Tls.
  (combine-out
   ClientConfig.default
   ClientConfig.certificates.set
   ServerConfig.default
   decodeCert.impl.v3
   decodePrivateKey
   handshake.impl.v3
   newServer.impl.v3
   newClient.impl.v3
   receive.impl.v3
   send.impl.v3
   terminate.impl.v3)))

; TODO check out the tests in here
; unison-src/transcripts-using-base/net.md

(define (decodePrivateKey text)
  text)
(define (decodeCert.impl.v3 text)
  text)
(define (ServerConfig.default certs key)
  (list certs key))
(define (ClientConfig.certificates.set config certs)
  (list config certs))
(define (newServer.impl.v3 config sock)
  (list config sock))

(define (ClientConfig.default host service-identification-suffix)
  (if (= 0 (bytes-length service-identification-suffix))
      (list host service-identification-suffix)
      (error 'NotImplemented "service-identification-suffix not supported")))

(define (handle-errors fn)
  (with-handlers
      [[exn:fail:network? (lambda (e) (exception "IOFailure" (exn->string e) '()))]
       [exn:fail:contract? (lambda (e) (exception "InvalidArguments" (exn->string e) '()))]
       [(lambda _ #t) (lambda (e) (exception "MiscFailure" "Unknown exception" e))] ]
    (fn)))

(define (newClient.impl.v3 config socket)
  (handle-errors
   (lambda ()
     (let ([input (car socket)]
           [output (car (cdr socket))]
           [hostname (car config)])
       (let-values ([(in out) (ports->ssl-ports
                               input output
                               #:hostname hostname
                               #:close-original? #t
                               )])
         (right (cons (cons in out) config)))))))

(define (handshake.impl.v3 tls)
  (handle-errors
   (lambda ()
     (ssl-set-verify! (car (car tls)) #t)
     (right none))))

(define (send.impl.v3 tls data)
  (handle-errors
   (lambda ()
     (let* ([ports (car tls)]
            [output (cdr ports)])
       (write-bytes data output)
       (right none)))))

(define (receive.impl.v3 tls)
  (handle-errors
   (lambda ()
     (right (port->bytes (car (car tls)) #:close? #f)))))

(define (terminate.impl.v3 tls)
  ; NOTE: This actually does more than the unison impl,
  ; which only sends the `close_notify` message, and doesn't
  ; mark the port as no longer usable in the runtime.
  ; Not sure if this is an important difference.
  ; Racket's openssl lib doesn't expose a way to *just* call
  ; SSL_Shutdown on a port without also closing it.
  (handle-errors
   (lambda ()
     (let ([ports (car tls)])
       (ssl-abandon-port (car ports))
       (ssl-abandon-port (cdr ports))
       (right none)))))


