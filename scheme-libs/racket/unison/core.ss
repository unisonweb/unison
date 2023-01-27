; This library implements various functions and macros that are used
; internally to the unison scheme libraries. This provides e.g. a
; measure of abstraction for the particular scheme platform. A useful
; feature of one implementation might need to be implemented on top of
; other features of another, and would go in this library.
;
; This library won't be directly imported by the generated unison
; code, so if some function is needed for those, it should be
; re-exported by (unison boot).
#!r6rs
(library (unison core)
  (export
    describe-value
    decode-value

    universal-compare

    fx1-
    list-head

    exception->string)

  (import (rnrs) (racket exn))

  (define (fx1- n) (fx- n 1))

  (define (list-head l n)
    (let rec ([c l] [m n])
      (cond
        [(eqv? m 0) '()]
        [(null? c) '()]
        [else
          (let ([sub (rec (cdr c) (- m 1))])
            (cons (car c) sub))])))

  (define (describe-value x) '())
  (define (decode-value x) '())

  ; 0 = LT
  ; 1 = EQ
  ; 2 = GT
  (define (universal-compare l r)
    (cond
      [(equal? l r) 1]
      [(and (number? l) (number? r)) (if (< l r) 0 2)]
      [else (raise "universal-compare: unimplemented")]))

  (define exception->string exn->string)

  )

