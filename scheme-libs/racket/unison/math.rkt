#lang racket/base

(require math/base
         (only-in unison/boot data-case define-unison))

(provide
    builtin-Float.exp
 (prefix-out unison-POp-
             (combine-out
              ABSF
              ACOS
              ACSH
              ADDF
              ADDI
              ASIN
              SINH
              TRNF
              ASNH
              ATAN
              ATN2
              ATNH
              CEIL
              EXPF
              COSF
              COSH
              DIVF
              DIVI
              EQLF
              EQLI
              SUBF
              LEQF
              SINF
              ITOF)))

(define-unison (builtin-Float.exp n) (exp n))
(define (EXPF n) (exp n))
(define ABSF abs)
(define ACOS acos)
(define ACSH acosh)
(define ADDF +)
(define ADDI +)
(define SUBF -)
(define ASIN asin)
(define ASNH asinh)
(define ATAN atan)
(define ATN2 atan)
(define ATNH atanh)
(define CEIL ceiling)
(define COSF cos)
(define TRNF truncate)
(define SINF sin)
(define SINH sinh)
(define COSH cosh)
(define DIVF /)
(define DIVI /)
(define ITOF exact->inexact)
(define (EQLF a b) (if (= a b) 1 0))
(define (LEQF a b) (if (<= a b) 1 0))
(define (EQLI a b) (if (= a b) 1 0))