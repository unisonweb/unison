#!racket/base

(provide
  (prefix-out
    builtin-
    (combine-out
      Nat.toFloat
      Nat.increment
      Float.*
      Float.fromRepresentation
      Float.toRepresentation
      Int.+
      Int.-
      Int.increment
      Int.negate
      Int.fromRepresentation
      Int.toRepresentation
      Int.signum
      )))

(require racket)
(require racket/fixnum)
(require racket/flonum)
(require racket/performance-hint)
(require unison/boot)

(define-unison (Nat.increment n) (add1 n))
(define-unison (Int.increment i) (add1 i))
(define-unison (Int.negate i) (fx- i))
(define-unison (Int.+ i j) (fx+ i j))
(define-unison (Int.- i j) (fx- i j))
(define-unison (Int.signum i) (sgn i))
(define-unison (Float.* x y) (fl* x y))

(define-unison (Nat.toFloat n) (->fl n))

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
