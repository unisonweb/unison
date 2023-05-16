; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/exn
         racket/string
         racket/file
         (only-in racket empty?)
         compatibility/mlist
         unison/data
         unison/chunked-seq
         unison/core
         unison/tcp
         unison/pem
         x509
         openssl)

(provide
 (prefix-out
  unison-FOp-Tls.
  (combine-out
   ClientConfig.default
   ClientConfig.certificates.set
   ServerConfig.default
   ServerConfig.certificates.set
   decodeCert.impl.v3
   decodePrivateKey
   encodePrivateKey
   handshake.impl.v3
   newServer.impl.v3
   newClient.impl.v3
   receive.impl.v3
   send.impl.v3
   terminate.impl.v3)))

; Native Representations:
;
; tlsPrivateKey - the "pem" struct defined in pem.rkt
; tlsCertificate - currently the raw bytes

(define (write-to-tmp-file bytes suffix)
  (let* ([tmp (make-temporary-file* #"unison" suffix)]
         [of (open-output-file tmp #:exists 'replace)])
    (write-bytes bytes of)
    (flush-output of)
    (close-output-port of)
    tmp))

(define (encodePrivateKey privateKey)
    (bytes->chunked-bytes (string->bytes/utf-8 (pem->pem-string privateKey))))

(define (decodePrivateKey bytes) ; bytes -> list tlsPrivateKey
  (vector->chunked-list
   (list->vector ; TODO better conversion
    (filter
     (lambda (pem) (or
                    (equal? "PRIVATE KEY" (pem-label pem))
                    (equal? "RSA PRIVATE KEY" (pem-label pem))))
     (pem-string->pems (bytes->string/utf-8 (chunked-bytes->bytes bytes)))))))

(define (decodeCert.impl.v3 bytes) ; bytes -> either failure tlsSignedCert
  (let ([certs (read-pem-certificates (open-input-bytes (chunked-bytes->bytes bytes)))])
    (if (= 1 (length certs))
        (right bytes)
        (exception "Wrong number of certs" (string->chunked-string "nope") certs)))) ; TODO passing certs is wrong, should either be converted to chunked-list or removed

(struct server-config (certs key)) ; certs = list certificate; key = privateKey

(define (ServerConfig.default certs key) ; list tlsSignedCert tlsPrivateKey -> tlsServerConfig
  (server-config certs key))

(struct client-config (host certs))
(struct tls (config input output))

(define (newServer.impl.v3 config socket-pair) ; tlsServerConfig socket -> {io} tls
  (handle-errors
   (lambda ()
     (let* ([input (socket-pair-input socket-pair)]
            [output (socket-pair-output socket-pair)]
            [certs (server-config-certs config)]
            [key (server-config-key config)]
            [key-bytes (string->bytes/utf-8 (pem->pem-string key))]
            [tmp (write-to-tmp-file (chunked-bytes->bytes (chunked-list-ref certs 0)) #".pem")])
       (let*-values ([(ctx) (ssl-make-server-context
                             ; TODO: Once racket can handle the in-memory PEM bytes,
                             ; we can do away with writing them out to temporary files.
                             ; https://github.com/racket/racket/pull/4625
                             ; #:private-key (list 'pem key-bytes)
                             #:private-key (list 'pem (write-to-tmp-file key-bytes #".pem"))
                             #:certificate-chain tmp)]
                     [(in out) (ports->ssl-ports
                                input output
                                #:mode 'accept
                                #:context ctx
                                #:close-original? #t
                                )])
         (right (tls config in out)))))))

(define (ClientConfig.default host service-identification-suffix) ; string bytes
  (if (= 0 (chunked-bytes-length service-identification-suffix))
      (client-config host empty-chunked-list)
      (error 'NotImplemented "service-identification-suffix not supported")))

(define (ServerConfig.certificates.set certs config)
  (server-config certs (server-config-key config)))

(define (ClientConfig.certificates.set certs config) ; list tlsSignedCert tlsClientConfig -> tlsClientConfig
  (client-config (client-config-host config) certs))

(define (handle-errors fn)
  (with-handlers
      [[exn:fail:network? (lambda (e) (exception "IOFailure" (exception->string e) '()))]
       [exn:fail:contract?
        (lambda (e) (exception "InvalidArguments" (exception->string e) '()))]
       [(lambda err
          (string-contains? (exn->string err) "not valid for hostname"))
        (lambda (e) (exception "IOFailure" (string->chunked-string "NameMismatch") '()))]
       [(lambda err
          (string-contains? (exn->string err) "certificate verify failed"))
        (lambda (e) (exception "IOFailure" (string->chunked-string "certificate verify failed") '()))]
       [(lambda _ #t) (lambda (e) (exception "MiscFailure" (string->chunked-string (format "Unknown exception ~a" (exn->string e))) e))]]
    (fn)))

(define (newClient.impl.v3 config socket)
  (handle-errors
   (lambda ()
     (let* ([input (socket-pair-input socket)]
           [output (socket-pair-output socket)]
           [hostname (client-config-host config)]
           ; TODO: Make the client context up in ClientConfig.default
           ; instead of right here.
           [ctx (ssl-make-client-context)]
           [certs (client-config-certs config)])
       (ssl-set-verify-hostname! ctx #t)
       (ssl-set-ciphers! ctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
       (ssl-set-verify! ctx #t)
       (if (chunked-list-empty? certs)
         (ssl-load-default-verify-sources! ctx)
         (let ([tmp (write-to-tmp-file (chunked-bytes->bytes (chunked-list-ref certs 0)) #".pem")])
            (ssl-load-verify-source! ctx tmp)))
       (let-values ([(in out) (ports->ssl-ports
                               input output
                               #:mode 'connect
                               #:context ctx
                               #:hostname (chunked-string->string hostname)
                               #:close-original? #t
                               )])
         (right (tls config in out)))))))

(define (handshake.impl.v3 tls)
  (handle-errors
   (lambda ()
     (ssl-set-verify! (tls-input tls) #t)
     (right none))))

(define (send.impl.v3 tls data) ; data = bytes
  (handle-errors
   (lambda ()
     (let* ([output (tls-output tls)])
       (write-bytes (chunked-bytes->bytes data) output)
       (flush-output output)
       (right none)))))

(define (read-more n port)
  (let* ([buffer (make-bytes n)]
         [read (read-bytes-avail! buffer port)])
    (if (< read n)
        (subbytes buffer 0 read)
        (bytes-append buffer (read-more (* 2 n) port)))))

(define (read-all n port)
  (let* ([buffer (make-bytes n)]
         [read (read-bytes-avail! buffer port)])
    (if (= n read)
        (bytes-append buffer (read-more (* 2 n) port))
        (subbytes buffer 0 read))))

(define (receive.impl.v3 tls) ; -> bytes
  (handle-errors
   (lambda ()
     (right (bytes->chunked-bytes (read-all 4096 (tls-input tls)))))))

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
