; TLS primitives! Supplied by openssl (libssl)
#lang racket/base
(require unison/data
         unison/core
         (only-in unison/data/chunked-seq
            bytes->chunked-bytes
            chunked-bytes->bytes)
         file/gunzip
         file/gzip)

(provide (prefix-out unison-FOp-Bytes.
    (combine-out
        zlib.compress
        zlib.decompress)))


(define (read-byte-only what i)
  (define c (read-byte i))
  (unless (byte? c)
    (error 'zlib "expected to get a byte for ~a, got enf-of-file" what))
  c)

(define (read-bytes-exactly what len i)
  (define bstr (read-bytes len i))
  (unless (and (bytes? bstr)
               (= (bytes-length bstr) len))
    (error 'zlib "error gettings bytes" what))
  bstr)

;; ADLER32 implementation
;; https://www.ietf.org/rfc/rfc1950.txt
;; Modified from racket/collects/net/git-checkout.rkt
(define (adler32-through-ports in out)
  (define ADLER 65521)
  (define bstr (make-bytes 4096))
  (let loop ([s1 1] [s2 0])
    (define n (read-bytes! bstr in))
    (cond
      [(eof-object? n)
       (bitwise-ior (arithmetic-shift s2 16) s1)]
      [else
       (write-bytes bstr out 0 n)
       (define-values (new-s1 new-s2)
         (for/fold ([s1 s1]
                    [s2 s2])
                   ([bits (in-bytes bstr 0 n)])
           (define a (modulo (+ s1 bits) ADLER))
           (define b (modulo (+ s2 a) ADLER))
           (values a b)))
       (loop new-s1 new-s2)])))

;; zlib-inflate : input-port output-port
;;  Reads compressed data from `i`, writes uncompressed to `o`
(define (zlib-inflate i o)
  (define cmf (read-byte-only 'zlib-cmf i))
  (define flg (read-byte-only 'zlib-flag i))
  (unless (= 8 (bitwise-and cmf #xF))
    (error 'zlib "compression is not `deflate`"))
  (when (bitwise-bit-set? flg 5)
    ;; read dictid
    (read-bytes-exactly 'dictid 4 i))
  ;; Include adler32 checksum in the pipeline, writing to `o`:
  (define-values (checksum-in checksum-out) (make-pipe 4096))
  (define uncompressed-adler #f)
  (define checksum-thread
    (thread
     (lambda () (set! uncompressed-adler (adler32-through-ports checksum-in o)))))
  ;; Inflate, sending output to checksum (and then to `o`):
  (inflate i checksum-out)
  (close-output-port checksum-out)
  (sync checksum-thread)
  ;; Verify checksum
  (define adler (read-bytes-exactly 'adler-checksum 4 i))
  (unless (= (integer-bytes->integer adler #f #t)
             uncompressed-adler)
    (error 'zlib "adler32 checksum failed"))
  (void))

(define (zlib-deflate i o)
    (write-byte #x78 o)
    (write-byte #x9c o)
    ;; Include adler32 checksum in the pipeline, writing to `o`:
    (define-values (checksum-in checksum-out) (make-pipe 4096))
    (define uncompressed-adler #f)
    (define checksum-thread
        (thread
     (lambda () (set! uncompressed-adler (adler32-through-ports i checksum-out)))))

    (sync checksum-thread)
    (close-output-port checksum-out)
    (deflate checksum-in o)
    ; (define adler (read-bytes-exactly 'adler-checksum 4 i))
    (write-bytes (integer->integer-bytes uncompressed-adler 4 #f #t) o)
    (void))

(define (zlib-deflate-bytes bytes)
    (let ([op1 (open-output-bytes)])
        (zlib-deflate (open-input-bytes bytes) op1)
        (get-output-bytes op1)))

(define (zlib-inflate-bytes bytes)
    (let ([op1 (open-output-bytes)])
        (zlib-inflate (open-input-bytes bytes) op1)
        (get-output-bytes op1)))

; (define op1 (open-output-bytes))
; (zlib-inflate (open-input-bytes (hex-string->bytes "789ccb48cdc9c95748cbcfc92e060019b10454")) op1)
; (display (get-output-bytes op1))
; (display "\n")

; (define op2 (open-output-bytes))
; (zlib-deflate (open-input-bytes #"hello folks") op2)
; (display (bytes->hex-string (get-output-bytes op2)))

(define (zlib.compress bytes)
    (bytes->chunked-bytes (zlib-deflate-bytes (chunked-bytes->bytes bytes))))

(define (zlib.decompress bytes)
    (with-handlers [[exn:fail? (lambda (e) (exception "Zlib data corrupted" (exception->string e) '()))] ]
        (right (bytes->chunked-bytes (zlib-inflate-bytes (chunked-bytes->bytes bytes))))))
