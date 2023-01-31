; This library wraps some implementation-specific functionality to
; provide immutable strings. Both have mechanisms for (efficiently)
; marking strings immutable, but there is no standard API for working
; entirely in terms of immutable strings. This module takes the
; freezing function, re-exported by (unison core) and implements the
; API needed for unison.
(library (unison string)
  (export
    istring
    istring-append
    istring-drop
    istring-take
    istring-repeat
    list->istring
    make-istring
    number->istring
    signed-number->istring
    utf8-bytevector->istring
    utf-8-transcoder)

  (import (rnrs) (unison core))

  (define istring (lambda l (freeze-string! (apply string l))))

  (define (make-istring n c) (freeze-string! (make-string n c)))

  (define (istring-repeat n s)
    (let* ([k (string-length s)]
           [t (make-string (* k n))])
      (let loop ([i 0])
        (if (< i n)
          (begin
            (string-copy! s 0 t (* i k) k)
            (loop (+ i 1)))
          (freeze-string! t)))))

  (define istring-append (lambda l (freeze-string! (apply string-append l))))

  (define (istring-drop n s) (freeze-string! (substring s n (- (string-length s) n))))

  (define (number->istring n) (freeze-string! (number->string n)))

  (define (signed-number->istring n)
    (freeze-string!
      (if (>= n 0)
        (string-append "+" (number->string n))
        (number->string n))))

  (define (list->istring l) (freeze-string! (list->string l)))

  (define (istring-take n s) (freeze-string! (substring s 0 n)))

  (define utf-8-transcoder (make-transcoder (utf-8-codec)))

  (define (utf8-bytevector->istring bs)
    (freeze-string! (bytevector->string bs utf-8-transcoder))))
