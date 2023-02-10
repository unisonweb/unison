;; Helpers for building data that conform to the compiler calling convention

#!r6rs
(library (unison data)
  (export
   some
   none
   some?
   none?
   option-get
   right
   left
   right?
   left?
   either-get
   either-get
   unit)

  (import (rnrs))

  ; Option a
  (define none (cons 0 ()))

  ; a -> Option a
  (define (some a) (cons 1 a))

  ; Option a -> Bool
  (define (some? option) (eq? 1 (car option)))

  ; Option a -> Bool
  (define (none? option) (eq? 0 (car option)))

  ; Option a -> a (or #f)
  (define (option-get option)
    (if
     (some? option)
     (cdr option)
     (raise "Cannot get the value of an empty option ")))

  ; Unit
  (define unit (cons 0 ()))

  ; a -> Either b a
  (define (right a)(cons 1 a))

  ; b -> Either b a
  (define (left b) (cons 0 b))

  ; Either a b -> Boolean
  (define (right? either) (eq? 1 (car either)))

  ; Either a b -> Boolean
  (define (left? either) (eq? 0 (car either)))

  ; Either a b -> a | b
  (define (either-get either) (cdr either)))
