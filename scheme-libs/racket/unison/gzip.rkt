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
         x509
         openssl)

(require file/gzip)
(require file/sha1)
(define op1 (open-output-bytes))

(define (>> x y) (arithmetic-shift x (- y)))

;; Assumes being called with c in 0..FF
(define (put_byte c)
    (write-byte c op1))

;; /* Output a 16 bit value, lsb first */
;; Assumes being called with c in 0..FFFF
(define (put_short w)
;   (if (< outcnt (- OUTBUFSIZ 2))
;       (begin (bytes-set! outbuf outcnt (bitwise-and #xFF w))
;              (bytes-set! outbuf (add1 outcnt) (>> w 8))
;              ;; this is not faster...
;              ;; (integer->integer-bytes w 2 #f #f outbuf outcnt)
;              (set! outcnt (+ outcnt 2)))
        (begin (put_byte (bitwise-and #xFF w))
                (put_byte (>> w 8)))); )

;; /* Output a 32 bit value to the bit stream, lsb first */
(define (put_long n)
    (put_short (bitwise-and #xFFFF n))
    (put_short (bitwise-and #xFFFF (>> n 16))))



;;;; This one, uses the crc32

; (put_byte #x78)
; (put_byte #x9c)
; (define-values
; (count __ crc)
; (deflate (open-input-bytes #"hello folks") op1 ))
; (display count)
; (display "\n")
; (display crc)
; (display "\n")
; (define (putShortMSB b)
;     (put_byte (bitwise-and (>> b 8) #xFF))
;     (put_byte (bitwise-and b #xFF)))
; (display (bytes->hex-string (get-output-bytes op1)))

(gzip-through-ports (open-input-bytes #"hello folks") op1 #f 0)

(display (bytes->hex-string (get-output-bytes op1)))


; (let-values ([(bytes_in two crc) (deflate (open-input-bytes #"hello folks") op1)])
;       (put_long (bitwise-xor (modulo crc 4294967296)  #xffff))
;       (put_long (bitwise-xor (modulo bytes_in 4294967296)  #xffff))
;       (display crc)
;       (display "\n")

;     (display (bytes->hex-string (get-output-bytes op1)))
;     )