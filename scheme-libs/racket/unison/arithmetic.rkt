#!racket/base

(provide
  (prefix-out
    builtin-
    (combine-out
      Nat.increment
      Float.fromRepresentation
      Float.toRepresentation
      Int.increment
      Int.fromRepresentation
      Int.toRepresentation
      )))

(require unison/boot)

(define-unison (Nat.increment n) (+ n 1))
(define-unison (Int.increment n) (+ n 1))

; If someone can suggest a better mechanism for these,
; that would be appreciated.
(define-unison (Float.toRepresentation fl)
  (integer-bytes->integer
    (real->floating-point-bytes fl 8 #t) ; big endian
    #f ; unsigned
    #t)) ; big endian

(define-unison (Float.fromRepresentation n)
  (floating-point-bytes->real
    (integer->integer-bytes n 8 #f #t) ; unsigned, big endian
    #t)) ; big endian

(define-unison (Int.toRepresentation i)
  (integer-bytes->integer
    (integer->integer-bytes i 8 #t #t) ; signed, big endian
    #f #t)) ; unsigned, big endian

(define-unison (Int.fromRepresentation n)
  (integer-bytes->integer
    (integer->integer-bytes n 8 #f #t) ; unsigned, big endian
    #t #t)) ; signed, big endian
