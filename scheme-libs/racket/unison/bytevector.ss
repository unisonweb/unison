; This library implements missing bytevector functionality for unison
; builtins. The main missing bits are better support for immutable
; bytevectors. Both chez and racket have support for immutable
; bytevectors, but there is no standard API for dealing with them that
; implements all the functions we'd want. This library exports the
; desired functionality on top of an unsafe in-place freeze
; re-exported from the (unison core) module.
#lang racket/base

(provide
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
  bytevector->base32-string
  base32-string->ibytevector)

(require
  racket
  racket/fixnum
  (only-in racket/unsafe/ops
           unsafe-bytes->immutable-bytes!)
  (only-in rnrs
           div
           mod
           div-and-mod
           bytevector-u8-ref
           bytevector-u16-ref
           bytevector-u32-ref
           bytevector-u64-ref))

(define freeze-bytevector! unsafe-bytes->immutable-bytes!)

(define (ibytevector-drop n bs)
  (let* ([l (bytes-length bs)]
         [k (max 0 (- l n))]
         [br (make-bytes k)])
    (bytes-copy! br 0 bs n k)
    (unsafe-bytes->immutable-bytes! br)))

(define (ibytevector-take n bs)
  (let* ([sz (min n (bytes-length bs))]
         [br (make-bytes sz)])
    (bytes-copy br 0 bs 0 sz)
    (unsafe-bytes->immutable-bytes! br)))

(define (ibytevector-append l r)
  (unsafe-bytes->immutable-bytes! (bytes-append l r)))

(define (u8-list->ibytevector l)
  (unsafe-bytes->immutable-bytes! (list->bytes l)))

(define (bytevector-u24-ref bs n end)
  (let ([v16 (bytevector-u16-ref bs n end)]
        [v8 (bytevector-u8-ref bs (+ n 2))])
    (case end
      [(big) (fxior v8 (fxlshift v16 8))]
      [(little) (fxior v16 (fxlshift v8 16))])))

(define (bytevector-u40-ref bs n end)
  (let ([v32 (bytevector-u32-ref bs n end)]
        [v8 (bytevector-u8-ref bs (+ n 4))])
    (case end
      [(big) (fxior v8 (fxlshift v32 8))]
      [(small) (fxior v32 (fxlshift v8 32))])))

(define (bytevector-u48-ref bs n end)
  (let ([v32 (bytevector-u32-ref bs n end)]
        [v16 (bytevector-u16-ref bs (+ n 4) end)])
    (case end
      [(big) (fxior v16 (fxlshift v32 8))]
      [(small) (fxior v32 (fxlshift v16 32))])))

(define (bytevector-u56-ref bs n end)
  (let ([v32 (bytevector-u32-ref bs n end)]
        [v16 (bytevector-u16-ref bs (+ n 4) end)]
        [v8 (bytevector-u8-ref bs (+ n 6))])
    (case end
      [(big) (fxior v8
                    (fxlshift v16 8)
                    (fxlshift v32 24))]
      [(small) (fxior v32
                      (fxlshift v16 32)
                      (fxlshift v8 48))])))

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

(define (base32-string->ibytevector str #:alphabet [alphabet 'standard])
  (define decode
    (match alphabet
      [hex b32hd]
      [standard b32d]))

  (define (main ilen)
    (let* ([olen (div (* ilen 5) 8)]
           [out (make-bytes olen)])

      (define (fill n k o)
        (when (>= k 0)
          (let ([m (fxand n 255)])
            (bytes-set! out (+ o k) m)
            (fill (fxrshift n 8) (- k 1) o))))

      (define (fixup i)
        (if (= i 0) (values 0 -1)
          (let* ([chars (+ 1 (mod (- i 1) 8))])
            (div-and-mod (* 5 chars) 8))))

      (let rec ([acc 0] [i 0] [o 0])
        (cond
          [(>= i ilen)
           (let-values ([(k n) (fixup i)])
             (fill (fxrshift acc n) (- k 1) o)
             (unsafe-bytes->immutable-bytes! out))]
          [(and (> i 0) (= 0 (mod i 8)))
           (fill acc 4 o)
           (rec (decode (string-ref str i)) (+ i 1) (+ o 5))]
          [else
            (let ([sacc (fxlshift acc 5)]
                  [bits (decode (string-ref str i))])
              (rec (fxior sacc bits) (+ i 1) o))]))))

  (let search ([i (- (string-length str) 1)])
    (if (and (>= i 0) (eq? (string-ref str i) #\=))
      (search (- i 1))
      (main (+ i 1)))))

; code should convert 5-bit numbers to the corresponding character
(define (bytevector->base32-string bs #:alphabet [alphabet 'standard])
  (define code
    (match alphabet
      [hex b32h]
      [standard b32]))

  (let* ([ilen (bytes-length bs)]
         [olen (* 8 (div (+ ilen 4) 5))]
         [out (make-string olen #\=)])

    (define (fill n k o)
      (if (>= k 0)
        (let ([m (fxand n 31)])
          (string-set! out (+ o k) (code m))
          (fill (fxrshift n 5) (- k 1) o))
        #f))

    (define (fixup i)
      (if (= i 0) (values 0 -1)
        (let ([bys (+ 1 (mod (- i 1) 5))])
          (let-values ([(d m) (div-and-mod (* 8 bys) 5)])
            (if (= m 0) (values m (- d 1))
              (values (- 5 m) d))))))

    (let rec ([acc 0] [i 0] [o 0])
      (cond
        [(>= i ilen)
         (let-values ([(n k) (fixup i)])
           (fill (fxlshift acc n) k o)
           out)]
        [(and (> i 0) (= 0 (mod i 5)))
         (fill acc 7 o)
         (rec (bytes-ref bs i) (+ i 1) (+ o 8))]
        [else
          (let ([sacc (fxlshift acc 8)]
                [by (bytes-ref bs i)])
            (rec (fxior sacc by) (+ i 1) o))]))))

; 65 = #\A
; 24 = #\2 - 26
(define (b32 n) (integer->char (+ n (if (< n 26) 65 24))))

; 48 = #\0
; 87 = #\a - 10
(define (b32h n) (integer->char (+ n (if (< n 10) 48 87))))

