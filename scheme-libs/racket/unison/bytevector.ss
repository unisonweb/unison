; This library implements missing bytevector functionality for unison
; builtins. The main missing bits are better support for immutable
; bytevectors. Both chez and racket have support for immutable
; bytevectors, but there is no standard API for dealing with them that
; implements all the functions we'd want. This library exports the
; desired functionality on top of an unsafe in-place freeze
; re-exported from the (unison core) module.
#!r6rs
(library (unison bytevector)
  (export
    freeze-bytevector!
    ibytevector-drop
    ibytevector-take
    ibytevector-append
    bytevector-u8-ref
    bytevector-u16-ref
    bytevector-u24-ref
    bytevector-u32-ref
    bytevector-u40-ref
    bytevector-u48-ref
    bytevector-u56-ref
    bytevector-u64-ref
    u8-list->ibytevector
    b32d
    b32hd
    base32-string->ibytevector)

  (import (rnrs)
          (unison core))

  (define (ibytevector-drop n bs)
    (let* ([l (bytevector-length bs)]
           [k (max 0 (- l n))]
           [br (make-bytevector k)])
      (bytevector-copy! bs n br 0 k)
      (freeze-bytevector! br)))

  (define (ibytevector-take n bs)
    (let* ([sz (min n (bytevector-length bs))]
           [br (make-bytevector sz)])
      (bytevector-copy! bs 0 br 0 sz)
      (freeze-bytevector! br)))

  (define (ibytevector-append l r)
    (freeze-bytevector! (bytevector-append l r)))

  (define (u8-list->ibytevector l)
    (freeze-bytevector! (u8-list->bytevector l)))

  (define (bytevector-u24-ref bs n end)
    (let ([v16 (bytevector-u16-ref bs n end)]
          [v8 (bytevector-u8-ref bs (+ n 2))])
      (case end
        [big (fxior v8 (fxarithmetic-shift-left v16 8))]
        [little (fxior v16 (fxarithmetic-shift-left v8 16))])))

  (define (bytevector-u40-ref bs n end)
    (let ([v32 (bytevector-u32-ref bs n end)]
          [v8 (bytevector-u8-ref bs (+ n 4))])
      (case end
        [big (fxior v8 (fxarithmetic-shift-left v32 8))]
        [small (fxior v32 (fxarithmetic-shift-left v8 32))])))

  (define (bytevector-u48-ref bs n end)
    (let ([v32 (bytevector-u32-ref bs n end)]
          [v16 (bytevector-u16-ref bs (+ n 4) end)])
      (case end
        [big (fxior v16 (fxarithmetic-shift-left v32 8))]
        [small (fxior v32 (fxarithmetic-shift-left v16 32))])))

  (define (bytevector-u56-ref bs n end)
    (let ([v32 (bytevector-u32-ref bs n end)]
          [v16 (bytevector-u16-ref bs (+ n 4) end)]
          [v8 (bytevector-u8-ref bs (+ n 6))])
      (case end
        [big (fxior v8
                    (fxarithmetic-shift-left v16 8)
                    (fxarithmetic-shift-left v32 24))]
        [small (fxior v32
                      (fxarithmetic-shift-left v16 32)
                      (fxarithmetic-shift-left v8 48))])))

  (define (b32d c)
    (let ([n (char->integer c)])
      (cond
        [(and (<= 65 n) (<= n 90)) (- n 65)]
        [(and (<= 97 n) (<= n 122)) (- n 97)]
        [(and (<= 50 n) (<= n 55)) (- n 24)])))

  (define (b32hd c)
    (let ([n (char->integer c)])
      (cond
        [(and (<= 48 n) (<= n 57)) (- n 48)]
        [(and (<= 65 n) (<= n 86)) (- n 65)]
        [(and (<= 97 n) (<= n 118)) (- n 97)])))

  (define (base32-string->ibytevector decode str)
    (define (main ilen)
      (let* ([olen (div (* ilen 5) 8)]
             [out (make-bytevector olen)])

        (define (fill n k o)
          (if (>= k 0)
            (let ([m (fxand n 255)])
              (bytevector-u8-set! out (+ o k) m)
              (fill (fxarithmetic-shift-right n 8) (- k 1) o))))

        (define (fixup i)
          (if (= i 0) (values 0 -1)
            (let ([chars (+ 1 (mod (- i 1) 8))])
              (div-and-mod (* 5 chars) 8))))

        (let rec ([acc 0] [i 0] [o 0])
          (cond
            [(>= i ilen)
             (let-values ([(k n) (fixup i)])
               (fill (fxarithmetic-shift-right acc n) (- k 1) o)
               (freeze-bytevector! out))]
            [(and (> i 0) (= 0 (mod i 8)))
             (fill acc 4 o)
             (rec (decode (string-ref str i)) (+ i 1) (+ o 5))]
            [else
              (let ([sacc (fxarithmetic-shift-left acc 5)]
                    [bits (decode (string-ref str i))])
                (rec (fxior sacc bits) (+ i 1) o))]))))

    (let search ([i (- (string-length str) 1)])
      (if (and (>= i 0) (eq? (string-ref str i) #\=))
        (search (- i 1))
        (main (+ i 1)))))


  )
