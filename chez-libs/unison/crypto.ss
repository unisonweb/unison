
(library (unison crypto)
  (export
    unison-FOp-crypto.HashAlgorithm.Sha1
    unison-FOp-crypto.hashBytes)

  (import (chezscheme)
          (unison core)
          (unison string)
          (unison bytevector))

  (define try-load-shared (lambda (name message)
    (guard (x [else (begin
      (printf "\n🚨🚨🚨 Unable to load shared library ~s🚨🚨🚨\n---> ~a\n\nOriginal exception:\n" name message)
      (raise x)
    )])
    (load-shared-object name)
    #t)))

  (define _libcrypto (try-load-shared "libcrypto.3.dylib" "Do you have openssl installed?"))
  (define _libb2 (try-load-shared "libb2.dylib" "Do you have libb2 installed?"))

  (define EVP_Digest
    (foreign-procedure "EVP_Digest"
      (
        u8*       ; input buffer
        unsigned-int ; length of input
        u8*          ; output buffer
        boolean      ; note: not a boolean, we just need to be able to pass NULL (0)
        void*        ; the EVP_MD* pointer, which holds the digest algorithm
        boolean      ; note: not a boolean, we just need to be able to pass NULL (0)
      )
      ; 1 if success, 0 or -1 for failure
      int))

  (define digest (lambda (text kind bits)
    (let ([buffer (make-bytevector (/ bits 8))])
      (if (= 1 (EVP_Digest text (bytevector-length text) buffer #f kind #f))
        buffer
        #f))))

  (define EVP_sha1 (foreign-procedure "EVP_sha1" () void*))
  (define EVP_sha256 (foreign-procedure "EVP_sha256" () void*))
  (define EVP_sha512 (foreign-procedure "EVP_sha512" () void*))
  (define EVP_sha3_256 (foreign-procedure "EVP_sha3_256" () void*))
  (define EVP_sha3_512 (foreign-procedure "EVP_sha3_512" () void*))

  (define sha1 (lambda (text) (digest text (EVP_sha1) 160)))
  (define sha256 (lambda (text) (digest text (EVP_sha256) 256)))
  (define sha512 (lambda (text) (digest text (EVP_sha512) 512)))
  (define sha3_256 (lambda (text) (digest text (EVP_sha3_256) 256)))
  (define sha3_512 (lambda (text) (digest text (EVP_sha3_512) 512)))

  (define blake2b-raw
    (foreign-procedure "blake2b"
      (
        u8* ; output buffer
        string ; input buffer
        u8* ; input key
        int ; output length
        int ; input length
        int ; key length
      ) int
    ))

  (define blake2s-raw
    (foreign-procedure "blake2s"
      (
        u8* ; output buffer
        string ; input buffer
        u8* ; input key
        int ; output length
        int ; input length
        int ; key length
      ) int
    ))

  (define blake2s (lambda (text size)
    (let ([buffer (make-bytevector (/ size 8))])
      (if (= 0 (blake2s-raw buffer text #f (/ size 8) (string-length text) 0))
        buffer
        #f))))

  (define blake2b (lambda (text size)
    (let ([buffer (make-bytevector (/ size 8))])
      (if (= 0 (blake2b-raw buffer text #f (/ size 8) (string-length text) 0))
        buffer
        #f))))

  (define (unison-FOp-crypto.HashAlgorithm.Sha1) sha1)
  (define (unison-FOp-crypto.hashBytes algo text)
    (algo text))

  )
