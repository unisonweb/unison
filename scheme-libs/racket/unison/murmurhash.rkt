#lang racket/base
; Ported from https://hackage.haskell.org/package/murmur-hash-0.1/docs/src/Data-Digest-Murmur64.html

(provide murmurhash-bytes)

(define max (- (expt 2 64) 1))
(define (int64 v)
    (let [(res (bitwise-and v max))]
        res))

(define (hash64End h)
    (let* [(h1 (int64 (bitwise-xor h (int64 (arithmetic-shift h murmur_r)))))
           (h2 (inexact->exact (int64 (* h1 murmur_m))))
           (h3 (int64 (bitwise-xor h2 (int64 (arithmetic-shift h2 murmur_r)))))]
        h3))

(define defaultSeed 3735928559)
(define murmur_m 14313749767032793493)
(define murmur_r -47)

(define (hash64AddWord64 word64 current-hash)
    (let* ([k1 (inexact->exact (int64 (* word64 murmur_m)))]
           [k_ (arithmetic-shift k1 murmur_r)]
           [k2 (int64 (bitwise-xor k1 (int64 k_)))]
           [k3 (inexact->exact (int64 (* k2 murmur_m)))]
           [h1 (inexact->exact (int64 (* current-hash murmur_m)))]
           [h2 (int64 (bitwise-xor h1 k3))])
        h2))

(define (hash64Add byteString current-hash)
    (let ([go (lambda [word current] (hash64AddWord64 word current))])
        (foldl go (hash64AddWord64 9 current-hash) (bytes->list byteString))
    )
)

(define (hash64WithSeed seed bytes)
    (hash64End (hash64Add bytes seed))
)

(define (murmurhash-bytes bytes) (hash64WithSeed defaultSeed bytes))

(module+ test
    (require rackunit
            (only-in openssl/sha1 bytes->hex-string hex-string->bytes))

;    1 | > Value.serialize (Value.value (Text.toUtf8 "hello"))
;          ⧩
;          0xs000000030304010568656c6c6f
;
;    2 | > murmurHash (Text.toUtf8 "hello")
;          ⧩
;          5800730648372668746

    (test-case "hash 'hello'"
        (check-equal?
            (murmurhash-bytes (hex-string->bytes "000000030304010568656c6c6f"))
            5800730648372668746)))
