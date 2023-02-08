#!r6rs

(library (unison concurrency)
  (export ok)

  (import (rnrs))

  (define none '(0))
  (define (some a) `(1 . ,a))
  (define (some? option) (eq? 1 (car option)))
  (define (get option) (cdr option))

  (define (ok) "getting started"))
