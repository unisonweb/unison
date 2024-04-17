#!racket/base

(provide
  (prefix-out
    builtin-
    (combine-out
      Nat.toFloat
      Nat.increment
      Nat.+
      Nat.drop
      Float.*
      Float.fromRepresentation
      Float.toRepresentation
      Float.ceiling
      Int.+
      Int.-
      Int./
      Int.increment
      Int.negate
      Int.fromRepresentation
      Int.toRepresentation
      Int.signum
      )))

(require racket
         racket/fixnum
         racket/flonum
         racket/performance-hint
         unison/boot)

(begin-encourage-inline
  (define-unison (Nat.+ m n) (clamp-natural (+ m n)))
  (define-unison (Nat.drop m n) (max 0 (- m n)))

  (define-unison (Nat.increment n) (clamp-natural (add1 n)))
  (define-unison (Int.increment i) (clamp-integer (add1 i)))
  (define-unison (Int.negate i) (if (> i nbit63) (- i) i))
  (define-unison (Int.+ i j) (clamp-integer (+ i j)))
  (define-unison (Int.- i j) (clamp-integer (- i j)))
  (define-unison (Int./ i j) (floor (/ i j)))
  (define-unison (Int.signum i) (sgn i))
  (define-unison (Float.* x y) (fl* x y))

  (define-unison (Nat.toFloat n) (->fl n))

  (define-unison (Float.ceiling f)
    (clamp-integer (fl->exact-integer (ceiling f))))

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
  )
