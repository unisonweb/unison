; This library wraps some chez functionality to provide immutable
; strings. There is a wrapper for making immutable strings in chez,
; but it copies its input, so relying on that would involve every
; string operation building a fresh mutable string, then making an
; immutable copy. This library instead directly freezes the newly
; created mutable strings.
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

  (import (chezscheme))

  (define (freeze-s! s)
    (($primitive $string-set-immutable!) s)
    s)

  (define istring (lambda l (freeze-s! (apply string l))))

  (define (make-istring n c) (freeze-s! (make-string n c)))

  (define (istring-repeat n s)
    (let* ([k (string-length s)]
           [t (make-string (* k n))])
      (let loop ([i 0])
        (if (< i n)
          (begin
            (string-copy! s 0 t (* i k) k)
            (loop (+ i 1)))
          (freeze-s! t)))))

  (define istring-append (lambda l (freeze-s! (apply string-append l))))

  (define (istring-drop n s) (freeze-s! (substring s n (- (string-length s) n))))

  (define (number->istring n) (freeze-s! (number->string n)))

  (define (signed-number->istring n)
    (freeze-s!
      (if (>= n 0)
        (string-append "+" (number->string n))
        (number->string n))))

  (define (list->istring l) (freeze-s! (list->string l)))

  (define (istring-take n s) (freeze-s! (substring s 0 n)))

  (define utf-8-transcoder (make-transcoder (utf-8-codec)))

  (define (utf8-bytevector->istring bs)
    (freeze-s! (bytevector->string bs utf-8-transcoder))))
