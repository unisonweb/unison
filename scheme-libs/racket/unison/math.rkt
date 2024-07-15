#lang racket/base

(require math/base
         racket/performance-hint
         rnrs/arithmetic/bitwise-6
         (only-in unison/boot
                  clamp-integer
                  clamp-natural
                  data-case
                  define-unison-builtin
                  nbit63))

(provide
  builtin-Float.exp
  builtin-Float.exp:termlink
  builtin-Float.log
  builtin-Float.log:termlink
  builtin-Float.max
  builtin-Float.max:termlink
  builtin-Float.min
  builtin-Float.min:termlink
  builtin-Float.tan
  builtin-Float.tan:termlink
  builtin-Float.tanh
  builtin-Float.tanh:termlink
  builtin-Float.logBase
  builtin-Float.logBase:termlink
  builtin-Int.*
  builtin-Int.*:termlink
  builtin-Int.pow
  builtin-Int.pow:termlink
  builtin-Int.trailingZeros
  builtin-Int.trailingZeros:termlink
  builtin-Nat.trailingZeros
  builtin-Nat.trailingZeros:termlink
  builtin-Int.popCount
  builtin-Int.popCount:termlink
  builtin-Nat.popCount
  builtin-Nat.popCount:termlink
  builtin-Float.pow
  builtin-Float.pow:termlink

 (prefix-out unison-POp-
             (combine-out
              ABSF
              ACOS
              ACSH
              ADDF
              ADDI
              LOGB
              ASIN
              SINH
              TRNF
              RNDF
              SQRT
              TANH
              TANF
              TZRO
              POPC
              ASNH
              ATAN
              ATN2
              ATNH
              CEIL
              FLOR
              EXPF
              COSF
              COSH
              MAXF
              MINF
              MULF
              MULI
              NEGI
              NTOF
              POWF
              POWI
              POWN
              DIVF
              DIVI
              EQLF
              EQLI
              SUBF
              SUBI
              SGNI
              LEQF
              SINF
              ITOF)))

(define-unison-builtin
  (builtin-Float.logBase base num)
  (log num base))
(define (LOGB base num) (log num base))

(define-unison-builtin
  (builtin-Float.exp n) (exp n))

(define-unison-builtin
  (builtin-Float.log n) (log n))

(define-unison-builtin
  (builtin-Float.max n m) (max n m))

(define-unison-builtin
  (builtin-Float.min n m) (min n m))

(define-unison-builtin
  (builtin-Float.tan n) (tan n))

(define-unison-builtin
  (builtin-Float.tanh n) (tanh n))

(define-unison-builtin
  (builtin-Int.* n m) (clamp-integer (* n m)))

(define-unison-builtin
  (builtin-Int.pow n m) (clamp-integer (expt n m)))

(define-unison-builtin
  (builtin-Int.trailingZeros n) (TZRO n))

(define-unison-builtin
  (builtin-Nat.trailingZeros n) (TZRO n))

(define-unison-builtin
  (builtin-Nat.popCount n) (POPC n))

(define-unison-builtin
  (builtin-Int.popCount n) (POPC n))

(define-unison-builtin
  (builtin-Float.pow n m) (expt n m))

(define (EXPF n) (exp n))
(define ABSF abs)
(define ACOS acos)
(define ACSH acosh)
(define ADDF +)
(define (ADDI i j) (clamp-integer (+ i j)))
(define SUBF -)
(define (SUBI i j) (clamp-integer (- i j)))
(define (SGNI n) (if (< n 0) -1 (if (> n 0) +1 0)))
(define MAXF max)
(define MINF min)
(define MULF *)
(define (MULI i j) (clamp-integer (* i j)))
(define (NEGI i) (if (> i nbit63) (- i) i))
(define NTOF exact->inexact)
(define POWF expt)
(define (POWI i j) (clamp-integer (expt i j)))
(define (POWN i j) (clamp-natural (expt i j)))
(define ASIN asin)
(define ASNH asinh)
(define ATAN atan)
(define ATN2 atan)
(define ATNH atanh)
(define CEIL ceiling)
(define FLOR floor)
(define COSF cos)
(define (TRNF f)
  (cond
    [(or (= f +inf.0) (= f -inf.0) (eqv? f +nan.0) (eqv? f +nan.f)) 0]
    [else (clamp-integer (inexact->exact (truncate f)))]))
(define RNDF round)
(define SQRT sqrt)
(define TANF tan)
(define TANH tanh)
(define SINF sin)
(define SINH sinh)
(define COSH cosh)
(define DIVF /)
(define (DIVI i j) (floor (/ i j)))
(define ITOF exact->inexact)
(define (EQLF a b) (if (= a b) 1 0))
(define (LEQF a b) (if (<= a b) 1 0))
(define (EQLI a b) (if (= a b) 1 0))

(define (POPC n)
  (modulo (bitwise-bit-count n) 65))

(define (TZRO n)
    (let ([bit (bitwise-first-bit-set n)])
        (if (eq? -1 bit)
            64
            bit)))
