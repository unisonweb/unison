#lang racket/base

(require unison/chunked-seq unison/data unison/boot)

(provide
 (rename-out [encodeNat16be unison-FOp-Bytes.encodeNat16be])
 (prefix-out
  unison-FOp-Bytes.
  (combine-out
   decodeNat16be
   decodeNat16le
   decodeNat32be
   decodeNat32le
   decodeNat64be
   decodeNat64le
   encodeNat16be
   encodeNat16le
   encodeNat32be
   encodeNat32le
   encodeNat64be
   encodeNat64le)))

(define (decodeNatBe bytes size)
  (if (< (chunked-bytes-length bytes) size)
      none
      (let ([buf (chunked-bytes->bytes bytes)])
        (define (loop acc n)
          (if (> n 0)
          (begin
              (loop
               (+
                (arithmetic-shift acc 8)
                (bytes-ref buf (- size n)))
               (- n 1))
          )
              acc
              ))
        (sum 1 (loop 0 size) (bytes->chunked-bytes
                              (subbytes buf size))))))

(define (decodeNatLe bytes size)
  (if (< (chunked-bytes-length bytes) size)
      none
      (let ([buf (chunked-bytes->bytes bytes)])
        (define (loop acc n)
          (if (> n 0)
              (loop
               (+
                (arithmetic-shift acc 8)
                (bytes-ref buf (- n 1)))
               (- n 1))
              acc))
        (sum 1 (loop 0 size) (bytes->chunked-bytes
                              (subbytes buf size))))))

(define (encodeNatBe num size)
  (define buf (make-bytes size 0))
  (define (loop x n)
    (when (> n 0)
      (bytes-set! buf (- n 1) (bitwise-and x 255))
      (loop (arithmetic-shift x -8) (- n 1))))
  (loop num size)
  (bytes->chunked-bytes buf))

(define (encodeNatLe num size)
  (define buf (make-bytes size 0))
  (define (loop x n)
    (when (> n 0)
      (bytes-set! buf (- size n) (bitwise-and x 255))
      (loop (arithmetic-shift x -8) (- n 1))))
  (loop num size)
  (bytes->chunked-bytes buf))

(define (encodeNat16be num) (encodeNatBe num 2))
(define (encodeNat16le num) (encodeNatLe num 2))
(define (encodeNat32be num) (encodeNatBe num 4))
(define (encodeNat32le num) (encodeNatLe num 4))
(define (encodeNat64be num) (encodeNatBe num 8))
(define (encodeNat64le num) (encodeNatLe num 8))

(define (decodeNat16be num) (decodeNatBe num 2))
(define (decodeNat16le num) (decodeNatLe num 2))
(define (decodeNat32be num) (decodeNatBe num 4))
(define (decodeNat32le num) (decodeNatLe num 4))
(define (decodeNat64be num) (decodeNatBe num 8))
(define (decodeNat64le num) (decodeNatLe num 8))