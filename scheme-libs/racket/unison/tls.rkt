; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/port
         racket/exn
         unison/data
         openssl)

(provide
 (prefix-out
  unison-FOp-Tls.
  (combine-out
   ClientConfig.default
   handshake.impl.v3
   newClient.impl.v3
   receive.impl.v3
   send.impl.v3
   terminate.impl.v3)))

; TODO check out the tests in here
; unison-src/transcripts-using-base/net.md

; NOTE: In racket, there's no place to use this service-identification-suffix
(define (ClientConfig.default host service-identification-suffix)
  (list host service-identification-suffix))

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
           [hostname (car config)]
           ; Unused
           [_service-identification-suffix (car (cdr config))])
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


