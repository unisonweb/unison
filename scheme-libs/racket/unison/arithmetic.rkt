#!racket/base

(provide
  builtin-Nat.+
  builtin-Nat.+:termlink
  builtin-Nat.toFloat
  builtin-Nat.toFloat:termlink
  builtin-Nat.increment
  builtin-Nat.increment:termlink
  builtin-Nat.drop
  builtin-Nat.drop:termlink
  builtin-Float.*
  builtin-Float.*:termlink
  builtin-Float.fromRepresentation
  builtin-Float.fromRepresentation:termlink
  builtin-Float.toRepresentation
  builtin-Float.toRepresentation:termlink
  builtin-Float.ceiling
  builtin-Float.ceiling:termlink
  builtin-Int.+
  builtin-Int.+:termlink
  builtin-Int.-
  builtin-Int.-:termlink
  builtin-Int./
  builtin-Int./:termlink
  builtin-Int.increment
  builtin-Int.increment:termlink
  builtin-Int.negate
  builtin-Int.negate:termlink
  builtin-Int.fromRepresentation
  builtin-Int.fromRepresentation:termlink
  builtin-Int.toRepresentation
  builtin-Int.toRepresentation:termlink
  builtin-Int.signum
  builtin-Int.signum:termlink)

(require racket
         racket/fixnum
         racket/flonum
         racket/performance-hint
         unison/data
         unison/boot)

(begin-encourage-inline
  (define-unison-builtin
    (builtin-Nat.+ m n)
    (clamp-natural (+ m n)))

  (define-unison-builtin
    (builtin-Nat.drop m n)
    (max 0 (- m n)))

  (define-unison-builtin
    (builtin-Nat.increment n)
    (clamp-natural (add1 n)))
  (define-unison-builtin
    (builtin-Int.increment i) (clamp-integer (add1 i)))
  (define-unison-builtin
    (builtin-Int.negate i) (if (> i nbit63) (- i) i))
  (define-unison-builtin
    (builtin-Int.+ i j) (clamp-integer (+ i j)))
  (define-unison-builtin
    (builtin-Int.- i j) (clamp-integer (- i j)))
  (define-unison-builtin
    (builtin-Int./ i j) (floor (/ i j)))
  (define-unison-builtin
    (builtin-Int.signum i) (sgn i))
  (define-unison-builtin
    (builtin-Float.* x y) (fl* x y))

  (define-unison-builtin
    (builtin-Nat.toFloat n) (->fl n))

  (define-unison-builtin
    (builtin-Float.ceiling f)
    (clamp-integer (fl->exact-integer (ceiling f))))

  ; If someone can suggest a better mechanism for these,
  ; that would be appreciated.
  (define-unison-builtin
    (builtin-Float.toRepresentation fl)
    (integer-bytes->integer
      (real->floating-point-bytes fl 8 #t) ; big endian
      #f ; unsigned
      #t)) ; big endian

  (define-unison-builtin
    (builtin-Float.fromRepresentation n)
    (floating-point-bytes->real
      (integer->integer-bytes n 8 #f #t) ; unsigned, big endian
      #t)) ; big endian

  (define-unison-builtin
    (builtin-Int.toRepresentation i)
    (integer-bytes->integer
      (integer->integer-bytes i 8 #t #t) ; signed, big endian
      #f #t)) ; unsigned, big endian

  (define-unison-builtin
    (builtin-Int.fromRepresentation n)
    (integer-bytes->integer
      (integer->integer-bytes n 8 #f #t) ; unsigned, big endian
      #t #t)) ; signed, big endian
  )
