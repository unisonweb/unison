; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require racket/exn
         racket/string
         racket/file
         (only-in racket empty?)
         compatibility/mlist
         unison/data
         unison/tcp
         unison/pem
         file/gzip
         x509
         openssl
         (only-in unison/data/chunked-seq
            bytes->chunked-bytes
            chunked-bytes->bytes))

(provide (prefix-out unison-FOp-Bytes.
    (combine-out
        gzip.compress
        gzip.decompress)))

(define (gzip-bytes bytes)
    (let ([op1 (open-output-bytes)])
        (gzip-through-ports (open-input-bytes bytes) op1 #f 0)
        (get-output-bytes op1)))

(define (gzip.compress bytes)
    (bytes->chunked-bytes (gzip-bytes (chunked-bytes->bytes bytes))))

(define (gzip.decompress bytes)
(right (bytes->chunked-bytes (gzip-bytes (chunked-bytes->bytes bytes)))))

; 0xs1f8b80000003cb48cdc9c95748cbcfc92e60cd32e08ab000
; 0xs1f8b800000013cb48cdc9c95748cbcfc92e60cd32e08ab000