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
         file/gunzip
         file/gzip
         file/sha1
         x509
         openssl)

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
(define (bytes-adler32 bstr)
  (define ADLER 65521)
  (define-values (s1 s2)
    (for/fold ([s1 1]
               [s2 0])
              ([bits (in-bytes bstr)])
      (define a (modulo (+ s1 bits) ADLER))
      (define b (modulo (+ s2 a) ADLER))
      (values a b)))
  ; (s2 << 16) | s1
  (bitwise-ior (arithmetic-shift s2 16) s1))

;; ADLER32 implementation
;; https://www.ietf.org/rfc/rfc1950.txt
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

(define op1 (open-output-bytes))
(zlib-inflate (open-input-bytes (hex-string->bytes "789ccb48cdc9c95748cbcfc92e060019b10454")) op1)
(display (get-output-bytes op1))
(display "\n")

(define op2 (open-output-bytes))
(zlib-deflate (open-input-bytes #"hello folks") op2)
(display (bytes->hex-string (get-output-bytes op2)))
