#lang racket/base

(require math/base
         rnrs/arithmetic/fixnums-6
         (only-in unison/boot data-case define-unison))

(provide
    builtin-Float.exp
    builtin-Float.log
    builtin-Float.max
    builtin-Float.min
    builtin-Float.tan
    builtin-Float.tanh
    builtin-Float.logBase
    builtin-Int.*
    builtin-Int.pow
    builtin-Int.trailingZeros
    builtin-Int.popCount
    builtin-Float.pow
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

(define-unison (builtin-Float.logBase base num) (log num base))
(define (LOGB base num) (log num base))
(define-unison (builtin-Float.exp n) (exp n))
(define-unison (builtin-Float.log n) (log n))
(define-unison (builtin-Float.max n m) (max n m))
(define-unison (builtin-Float.min n m) (min n m))
(define-unison (builtin-Float.tan n) (tan n))
(define-unison (builtin-Float.tanh n) (tanh n))
(define-unison (builtin-Int.* n m) (* n m))
(define-unison (builtin-Int.pow n m) (expt n m))
(define-unison (builtin-Int.trailingZeros n) (TZRO n))
(define-unison (builtin-Int.popCount n) (POPC n))
(define-unison (builtin-Float.pow n m) (expt n m))
(define (EXPF n) (exp n))
(define ABSF abs)
(define ACOS acos)
(define ACSH acosh)
(define ADDF +)
(define ADDI +)
(define SUBF -)
(define SUBI -)
(define (SGNI n) (if (< n 0) -1 (if (> n 0) +1 0)))
(define MAXF max)
(define MINF min)
(define MULF *)
(define MULI *)
(define NEGI -)
(define NTOF exact->inexact)
(define POWF expt)
(define POWI expt)
(define POWN expt)
(define ASIN asin)
(define ASNH asinh)
(define ATAN atan)
(define ATN2 atan)
(define ATNH atanh)
(define CEIL ceiling)
(define FLOR floor)
(define COSF cos)
(define TRNF truncate)
(define RNDF round)
(define SQRT sqrt)
(define TANF tan)
(define TANH tanh)
(define SINF sin)
(define SINH sinh)
(define COSH cosh)
(define DIVF /)
(define DIVI /)
(define ITOF exact->inexact)
(define (EQLF a b) (if (= a b) 1 0))
(define (LEQF a b) (if (<= a b) 1 0))
(define (EQLI a b) (if (= a b) 1 0))

(define (POPC n)
    (if (< n 0)
        (+ 65 (fxbit-count n))
        (fxbit-count n)))

(define (TZRO n)
    (let ([bit (fxfirst-bit-set n)])
        (if (eq? -1 bit)
            64
            bit)))

; (do ((result 0 (+ result 1))
;      (bits (if (fxnegative? fx)
;                (fxnot fx)
;                fx)
;            (fxarithmetic-shift-right bits 1)))
;     ((fxzero? bits)
;      result))

; fxfirst-bit-set
