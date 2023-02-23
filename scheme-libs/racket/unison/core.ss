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

    syntax->list
    raise-syntax-error

    exception->string
    let-marks
    ref-mark

    freeze-string!
    string-copy!

    freeze-bytevector!
    freeze-vector!
    
    bytevector)

  (import
    (rnrs)
    (rename (only (racket)
                  string-copy!
                  bytes
                  with-continuation-mark
                  continuation-mark-set-first
                  raise-syntax-error)
            (string-copy! racket-string-copy!)
            (bytes bytevector))
    (racket exn)
    (racket unsafe ops))

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

  (define (syntax->list stx)
    (syntax-case stx ()
      [() '()]
      [(x . xs) (cons #'x (syntax->list #'xs))]))

  (define (call-with-marks rs v f)
    (cond
      [(null? rs) (f)]
      [else
        (with-continuation-mark (car rs) v
          (call-with-marks (cdr rs) v f))]))

  (define-syntax let-marks
    (syntax-rules ()
      [(let-marks ks bn e ...)
       (call-with-marks ks bn (lambda () e ...))]))

  (define (ref-mark k) (continuation-mark-set-first #f k))

  (define freeze-string! unsafe-string->immutable-string!)
  (define freeze-bytevector! unsafe-bytes->immutable-bytes!)

  (define freeze-vector! unsafe-vector*->immutable-vector!)

  ; racket string-copy! has the opposite argument order convention
  ; from chez.
  (define (string-copy! src soff dst doff len)
    (racket-string-copy! dst doff src soff len))
  )
