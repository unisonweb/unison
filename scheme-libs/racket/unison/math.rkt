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

(define (LOGB base num) (log num base))

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
