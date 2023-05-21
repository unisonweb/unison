#lang racket/base

(require unison/chunked-seq)

(provide
    (rename-out [encodeNat16be unison-FOp-Bytes.encodeNat16be])
 (prefix-out
  unison-FOp-Bytes.
  (combine-out
    encodeNat16be
    encodeNat16le
    encodeNat32be
    encodeNat32le
    encodeNat64be
    encodeNat64le)))

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
