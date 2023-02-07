#lang racket/base

(require racket crypto crypto/libcrypto crypto/b2)

(provide
    (prefix-out 
    unison-FOp-crypto.
    (except-out (all-defined-out) lc b2)
    ))

(crypto-factories (list libcrypto-factory b2-factory))

(define (lc sym)
    (unless (send libcrypto-factory get-version)
        (send libcrypto-factory print-lib-info)
        (error sym "Unable to load libcrypto"))
    sym)

(define (b2 sym)
    (unless (send b2-factory get-version)
        (send b2-factory print-lib-info)
        (error sym "Unable to load libb2"))
    sym)

(define (HashAlgorithm.Sha1) (lc 'sha1))
(define (HashAlgorithm.Sha2_256) (lc 'sha256))
(define (HashAlgorithm.Sha2_512) (lc 'sha512))
(define (HashAlgorithm.Sha3_256) (lc 'sha3-256))
(define (HashAlgorithm.Sha3_512) (lc 'sha3-512))
(define (HashAlgorithm.Blake2b_256) (b2 'blake2b-256))
(define (HashAlgorithm.Blake2s_256) (b2 'blake2s-256))
(define (HashAlgorithm.Blake2b_512) (b2 'blake2b-512))
(define hashBytes digest)
