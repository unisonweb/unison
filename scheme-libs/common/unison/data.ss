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
   unit
   false
   true)

  (import (rnrs))

  ; Option a
  (define none `(0))

  ; a -> Option a
  (define (some a) `(1 ,a))

  ; Option a -> Bool
  (define (some? option) (eq? 1 (car option)))

  ; Option a -> Bool
  (define (none? option) (eq? 0 (car option)))

  ; Option a -> a (or #f)
  (define (option-get option)
    (if
     (some? option)
     (car (cdr option))
     (raise "Cannot get the value of an empty option ")))

  ; TODO this might be reduntant, #<void> works
  ; Unit
  (define unit (cons 0 ()))

  ; Booleans are represented as numbers
  (define false 0)
  (define true 1)

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
