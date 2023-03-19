; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/port
         racket/exn
         racket/file
         compatibility/mlist
         unison/data
         x509
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

(define (decodePrivateKey bytes) ; bytes -> list tlsPrivateKey
  (let* ([tmp (make-temporary-file* #"unison" #".pem")]
         [ctx (ssl-make-server-context)]
         [of (open-output-file tmp #:exists 'replace)])
    (write-bytes bytes of)
    (flush-output of)
    (close-output-port of)
    (with-handlers
        [[exn:fail? (lambda (e) (mlist))]]
      (ssl-load-private-key! ctx tmp)
      (mlist tmp))))

(define (decodeCert.impl.v3 bytes) ; bytes -> either failure tlsSignedCert
  (let ([certs (read-pem-certificates (open-input-bytes bytes))])
    (if (= 1 (length certs))
        (right bytes)
        (exception "Wrong number of certs" "nope" certs))))

(struct server-config (certs key))

(define (ServerConfig.default certs key) ; list tlsSignedCert tlsPrivateKey -> tlsServerConfig
  (display "Making a config\n")
  (display certs)
  (server-config certs key))

(struct client-config (host certs))
(struct tls (config input output))
; (struct server (conf input output))

(define (newServer.impl.v3 config sockets) ; tlsServerConfig socket -> {io} tls
  (display "Are we at a sever\n")
  (handle-errors
   (lambda ()
     (let* ([input (car sockets)]
            [output (car (cdr sockets))]
            [certs (server-config-certs config)]
            [key (server-config-key config)]
            ; [ctx (ssl-make-server-context)]
            [tmp (make-temporary-file* #"unison" #".pem")]
            [of (open-output-file tmp #:exists 'replace)])
       (display "Um\n")
       (display certs)
       (display "\n")
       (display of)
       (display "\nWriting bytes\n")
       ; START HERE: I need to get the items out of certs,
       ; but I'm not sure how.
       (write-bytes (mcar certs) of)
       (flush-output of)
       (close-output-port of)
       ;    (ssl-load-private-key! ctx (car certs))
       ;    (ssl-load-certificate-chain! ctx tmp)
       (display "server booting up\n")
       (let*-values (
                     [(ctx) (ssl-make-server-context
                             #:private-key (list 'pem key)
                             #:certificate-chain tmp)]
                     [(in out) (ports->ssl-ports
                                input output
                                #:mode 'accept
                                #:context ctx
                                #:close-original? #t
                                )])
         (display "server happened\n")
         (right (tls config in out)))))))

(define (ClientConfig.default host service-identification-suffix)
  (if (= 0 (bytes-length service-identification-suffix))
      (client-config host (mlist))
      (error 'NotImplemented "service-identification-suffix not supported")))

(define (ClientConfig.certificates.set certs config) ; list tlsSignedCert tlsClientConfig -> tlsClientConfig
  (client-config (client-config-host config) certs))
;   (list (car config) certs))

(define (handle-errors fn)
  (with-handlers
      [[exn:fail:network? (lambda (e) (display e)(display "GOT AN ERROR\n") (exception "IOFailure" (exn->string e) '()))]
       [exn:fail:contract? (lambda (e)  (display e)(display "GOT AN ERROR\n")(exception "InvalidArguments" (exn->string e) '()))]
       [(lambda _ #t) (lambda (e)  (display e)(display "GOT AN ERROR\n")(exception "MiscFailure" "Unknown exception" e))] ]
    (fn)))

(define (newClient.impl.v3 config socket)
  (handle-errors
   (lambda ()
     (let ([input (car socket)]
           [output (car (cdr socket))]
           [hostname (client-config-host config)])
       (display "um got things\n")
       (let-values ([(in out) (ports->ssl-ports
                               input output
                               #:mode 'connect ; WAIT This was defaulting to accept and it still worked connecting to example.com???
                               ; maybe I should try connecting to unisonweb.org or something
                               #:hostname hostname
                               #:close-original? #t
                               )])
         (display "ports are ported\n")
         (right (tls config in out)))))))

(define (handshake.impl.v3 tls)
  (handle-errors
   (lambda ()
     (ssl-set-verify! (tls-input tls) #t)
     (right none))))

(define (send.impl.v3 tls data)
  (handle-errors
   (lambda ()
     (let* ([output (tls-output tls)])
       (write-bytes data output)
       (flush-output output)
       (right none)))))

(define (receive.impl.v3 tls)
  (handle-errors
   (lambda ()
     (let ([buffer (make-bytes 4096)])
       (read-bytes-avail! buffer (tls-input tls))
       (right buffer)))))
;  (right (port->bytes (car (car tls)) #:close? #f))

(define (terminate.impl.v3 tls)
  ; NOTE: This actually does more than the unison impl,
  ; which only sends the `close_notify` message, and doesn't
  ; mark the port as no longer usable in the runtime.
  ; Not sure if this is an important difference.
  ; Racket's openssl lib doesn't expose a way to *just* call
  ; SSL_Shutdown on a port without also closing it.
  (handle-errors
   (lambda ()
     (ssl-abandon-port (tls-input tls))
     (ssl-abandon-port (tls-output tls))
     (right none))))
