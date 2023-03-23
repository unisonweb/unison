#!r6rs
;; stubbed out just to avoid import error, replace with real thing
(library (unison crypto)
  (export
    unison-FOp-crypto.HashAlgorithm.Sha1
    unison-FOp-crypto.hashBytes)

  (import (rnrs))

  (define (unison-FOp-crypto.HashAlgorithm.Sha1) (lambda (x) x))
  (define (unison-FOp-crypto.hashBytes algo text) (algo text)))
