#lang racket/base

(require racket/contract
         racket/fixnum
         racket/list
         "chunked-seq.rkt")

(provide
    (prefix-out unison-FOp-
    (combine-out
        toBase16
        toBase32
        toBase64
        fromBase16
        fromBase32
        fromBase64
        toBase64UrlUnpadded
        fromBase64UrlUnpadded))
    (contract-out
          [base16-encode (-> chunked-bytes? chunked-bytes?)]
          [base16-decode (->* [chunked-bytes?] [#:fail (-> string? any)] any)]
          [base32-encode (->* [chunked-bytes?] [#:alphabet (or/c 'standard 'hex)] chunked-bytes?)]
          [base32-decode (->* [chunked-bytes?]
                              [#:alphabet (or/c 'standard 'hex)
                               #:fail (-> string? any)]
                              any)]
          [base64-encode (->* [chunked-bytes?] [#:pad? any/c] chunked-bytes?)]
          [base64-decode (->* [chunked-bytes?]
                              [#:padded? any/c
                               #:fail (-> string? any)]
                              any)]))

;; -----------------------------------------------------------------------------

(define ESCAPE-PROMPT-TAG (make-continuation-prompt-tag 'base64-fail))

(define (call-with-escape-handler handler proc)
  (call-with-continuation-prompt proc ESCAPE-PROMPT-TAG handler))

(define (escape msg)
  (abort-current-continuation ESCAPE-PROMPT-TAG msg))

(define (escape/invalid-encoding offset)
  (escape (format "input: invalid encoding at offset: ~a" offset)))

(define (ascii-range start end)
  (inclusive-range (char->integer start) (char->integer end)))

;; -----------------------------------------------------------------------------
;; base16

(define BASE16-DIGIT=>CHAR (apply vector-immutable (append (ascii-range #\0 #\9) (ascii-range #\a #\z))))
(define BASE16-CHAR=>DIGIT
  (let ([vec (make-vector 256 #f)])
    (for ([i (in-range 10)])
      (vector-set! vec (+ (char->integer #\0) i) i))
    (for ([i (in-range 6)])
      (vector-set! vec (+ (char->integer #\A) i) (+ 10 i))
      (vector-set! vec (+ (char->integer #\a) i) (+ 10 i)))
    (vector->immutable-vector vec)))

(define (base16-encode bs)
  (define in-len (chunked-bytes-length bs))
  (cond
    [(zero? in-len)
     empty-chunked-bytes]
    [else
     (define-values [more-bytes? next-byte] (sequence-generate (in-chunked-bytes bs)))
     (define last-byte #f)
     (build-chunked-bytes
      (* in-len 2)
      (λ (i)
        (define last-b last-byte)
        (vector-ref
         BASE16-DIGIT=>CHAR
         (cond
           [last-b
            (set! last-byte #f)
            (fxand last-b #xf)]
           [else
            (define b (next-byte))
            (set! last-byte b)
            (fxrshift b 4)]))))]))

(define (base16-decode bs #:fail [fail (λ (msg) (error 'base16-decode "~a" msg))])
  (define in-len (chunked-bytes-length bs))
  (cond
    [(zero? in-len)
     empty-chunked-bytes]
    [else
     (call-with-escape-handler
      fail
      (λ ()
        (define-values [out-len leftover-nibbles] (quotient/remainder in-len 2))
        (unless (zero? leftover-nibbles)
          (escape "input: invalid length"))

        (define-values [more-bytes? next-byte] (sequence-generate (in-chunked-bytes bs)))
        (define input-pos 0)
        (define (next-nibble)
          (begin0
            (or (vector-ref BASE16-CHAR=>DIGIT (next-byte))
                (escape/invalid-encoding input-pos))
            (set! input-pos (fx+ input-pos 1))))

        (build-chunked-bytes
         out-len
         (λ (i) (fxior (fxlshift (next-nibble) 4) (next-nibble))))))]))

;; -----------------------------------------------------------------------------
;; base32

(define BASE32-DIGIT=>CHAR (apply vector-immutable (append (ascii-range #\A #\Z) (ascii-range #\2 #\7))))
(define BASE32-CHAR=>DIGIT
  (let ([vec (make-vector 256 #f)])
    (for ([i (in-range 26)])
      (vector-set! vec (+ (char->integer #\A) i) i))
    (for ([i (in-range 6)])
      (vector-set! vec (+ (char->integer #\2) i) (+ 26 i)))
    (vector->immutable-vector vec)))

(define BASE32-HEX-DIGIT=>CHAR (apply vector-immutable (append (ascii-range #\0 #\9) (ascii-range #\a #\v))))
(define BASE32-HEX-CHAR=>DIGIT
  (let ([vec (make-vector 256 #f)])
    (for ([i (in-range 10)])
      (vector-set! vec (+ (char->integer #\0) i) i))
    (for ([i (in-range 22)])
      (vector-set! vec (+ (char->integer #\A) i) (+ 10 i))
      (vector-set! vec (+ (char->integer #\a) i) (+ 10 i)))
    (vector->immutable-vector vec)))

(define (base32-encode bs #:alphabet [alphabet 'standard])
  (define in-len (chunked-bytes-length bs))
  (cond
    [(zero? in-len)
     empty-chunked-bytes]
    [else
     (define-values [full-groups leftover-bytes] (quotient/remainder in-len 5))
     (define out-len (fx* (if (zero? leftover-bytes) full-groups (fx+ full-groups 1)) 8))

     (define digit=>char
       (case alphabet
         [(standard) BASE32-DIGIT=>CHAR]
         [(hex)      BASE32-HEX-DIGIT=>CHAR]))

     (define-values [more-input-bytes? next-input-byte] (sequence-generate (in-chunked-bytes bs)))
     (define last-input-byte #f)

     ;; Stores the index of the next output byte, modulo 8. As a
     ;; special case, a value of #f means the input is fully-consumed
     ;; and we’re just writing padding bytes (if any).
     (define output-state 0)

     (define (next-output-byte/from-last next-state shift)
       (set! output-state next-state)
       (vector-ref digit=>char (fxand (fxrshift last-input-byte shift) #b11111)))

     (define (next-output-byte/combine next-state mask l-shift r-shift)
       (define b1 last-input-byte)
       (define b2 (and (more-input-bytes?) (next-input-byte)))
       (set! last-input-byte b2)
       (set! output-state (and b2 next-state))
       (vector-ref digit=>char (fxior (fxlshift (fxand b1 mask) l-shift) (fxrshift (or b2 0) r-shift))))

     (define (next-output-byte)
       (case output-state
         [(0)
          (define b1 (next-input-byte))
          (set! last-input-byte b1)
          (set! output-state 1)
          (vector-ref digit=>char (fxrshift b1 3))]
         [(1) (next-output-byte/combine   2 #b111 2 6)]
         [(2) (next-output-byte/from-last 3 1)]
         [(3) (next-output-byte/combine   4 #b1 4 4)]
         [(4) (next-output-byte/combine   5 #b1111 1 7)]
         [(5) (next-output-byte/from-last 6 2)]
         [(6) (next-output-byte/combine   7 #b11 3 5)]
         [(7)
          (set! output-state 0)
          (vector-ref digit=>char (fxand last-input-byte #b11111))]
         [else
          (char->integer #\=)]))

     (build-chunked-bytes out-len (λ (i) (next-output-byte)))]))

(define (base32-decode bs
                       #:alphabet [alphabet 'standard]
                       #:fail [fail (λ (msg) (error 'base32-decode "~a" msg))])
  (define in-len (chunked-bytes-length bs))
  (cond
    [(zero? in-len)
     empty-chunked-bytes]
    [else
     (call-with-escape-handler
      fail
      (λ ()
        (define-values [full-groups leftover-bytes] (quotient/remainder in-len 8))
        (unless (zero? leftover-bytes)
          (escape "input: invalid length"))

        (define padding-len
          (let loop ([i 0])
            (if (or (= i 6)
                    (not (= (chunked-bytes-ref bs (fx- in-len i 1))
                            (char->integer #\=))))
                i
                (loop (fx+ i 1)))))

        (define out-len
          (fx- (fx* full-groups 5)
               (case padding-len
                 [(0) 0]
                 [(1) 1]
                 [(2) (escape/invalid-encoding (fx- in-len 3))]
                 [(3) 2]
                 [(4) 3]
                 [(5) (escape/invalid-encoding (fx- in-len 6))]
                 [(6) 4])))

        (define char=>digit
          (case alphabet
            [(standard) BASE32-CHAR=>DIGIT]
            [(hex)      BASE32-HEX-CHAR=>DIGIT]))

        (define-values [more-input-bytes? next-input-byte] (sequence-generate (in-chunked-bytes bs)))
        (define input-pos 0)
        (define (next-input-digit)
          (define digit (vector-ref char=>digit (next-input-byte)))
          (cond
            [digit
             (set! input-pos (fx+ input-pos 1))
             digit]
            [else
             (escape/invalid-encoding input-pos)]))

        (define last-input-digit #f)

        ;; Stores the index of the next output byte, modulo 5.
        (define out-state 0)

        (define (next-output-byte)
          (case out-state
            [(0)
             (define d1 (next-input-digit))
             (define d2 (next-input-digit))
             (set! last-input-digit d2)
             (set! out-state 1)
             (fxior (fxlshift d1 3) (fxrshift d2 2))]
            [(1)
             (define d2 last-input-digit)
             (define d3 (next-input-digit))
             (define d4 (next-input-digit))
             (set! last-input-digit d4)
             (set! out-state 2)
             (fxior (fxlshift (fxand d2 #b11) 6) (fxlshift d3 1) (fxrshift d4 4))]
            [(2)
             (define d4 last-input-digit)
             (define d5 (next-input-digit))
             (set! last-input-digit d5)
             (set! out-state 3)
             (fxior (fxlshift (fxand d4 #b1111) 4) (fxrshift d5 1))]
            [(3)
             (define d5 last-input-digit)
             (define d6 (next-input-digit))
             (define d7 (next-input-digit))
             (set! last-input-digit d7)
             (set! out-state 4)
             (fxior (fxlshift (fxand d5 #b1) 7) (fxlshift d6 2) (fxrshift d7 3))]
            [else
             (define d7 last-input-digit)
             (define d8 (next-input-digit))
             (set! out-state 0)
             (fxior (fxlshift (fxand d7 #b111) 5) d8)]))

        (build-chunked-bytes out-len (λ (i) (next-output-byte)))))]))

;; -----------------------------------------------------------------------------
;; base64

(define BASE64-DIGIT=>CHAR
  (apply vector-immutable
         (append (ascii-range #\A #\Z)
                 (ascii-range #\a #\z)
                 (ascii-range #\0 #\9)
                 (list (char->integer #\+)
                       (char->integer #\/)))))

(define BASE64-CHAR=>DIGIT
  (let ([vec (make-vector 256 #f)])
    (for ([i (in-range 26)])
      (vector-set! vec (+ (char->integer #\A) i) i)
      (vector-set! vec (+ (char->integer #\a) i) (+ 26 i)))
    (for ([i (in-range 10)])
      (vector-set! vec (+ (char->integer #\0) i) (+ 52 i)))
    (vector-set! vec (char->integer #\+) 62)
    (vector-set! vec (char->integer #\/) 63)
    (vector->immutable-vector vec)))

(define (base64-encode bs #:pad? [pad? #t])
  (define in-len (chunked-bytes-length bs))
  (cond
    [(zero? in-len)
     empty-chunked-bytes]
    [else
     (define-values [full-triplets leftover-bytes] (quotient/remainder in-len 3))
     (define out-len (fx+ (fx* full-triplets 4)
                          (cond
                            [(fx= leftover-bytes 0) 0]
                            [pad?                   4]
                            [(fx= leftover-bytes 1) 2]
                            [else                   3])))

     (define-values [more-input-bytes? next-input-byte] (sequence-generate (in-chunked-bytes bs)))
     (define last-input-byte #f)

     ;; Stores the index of the next output byte, modulo 4. As a
     ;; special case, a value of #f means the input is fully-consumed
     ;; and we’re just writing padding bytes (if any).
     (define output-state 0)

     (define (next-output-byte)
       (case output-state
         [(0)
          (define b1 (next-input-byte))
          (set! last-input-byte b1)
          (set! output-state 1)
          (vector-ref BASE64-DIGIT=>CHAR (fxrshift b1 2))]
         [(1)
          (define b1 last-input-byte)
          (define b2 (and (more-input-bytes?) (next-input-byte)))
          (set! last-input-byte b2)
          (set! output-state (and b2 2))
          (vector-ref BASE64-DIGIT=>CHAR (fxior (fxlshift (fxand b1 #b11) 4) (fxrshift (or b2 0) 4)))]
         [(2)
          (define b2 last-input-byte)
          (define b3 (and (more-input-bytes?) (next-input-byte)))
          (set! last-input-byte b3)
          (set! output-state (and b3 3))
          (vector-ref BASE64-DIGIT=>CHAR (fxior (fxlshift (fxand b2 #b1111) 2) (fxrshift (or b3 0) 6)))]
         [(3)
          (define b3 last-input-byte)
          (set! output-state 0)
          (vector-ref BASE64-DIGIT=>CHAR (fxand b3 #b111111))]
         [else
          (char->integer #\=)]))

     (build-chunked-bytes out-len (λ (i) (next-output-byte)))]))

(define (base64-decode bs
                       #:padded? [padded? #t]
                       #:fail [fail (λ (msg) (error 'base64-decode "~a" msg))])
  (define in-len (chunked-bytes-length bs))
  (cond
    [(zero? in-len)
     empty-chunked-bytes]
    [else
     (call-with-escape-handler
      fail
      (λ ()
        (define-values [full-quartets leftover-bytes] (quotient/remainder in-len 4))
        (when (if padded?
                  (not (zero? leftover-bytes))
                  (= leftover-bytes 1))
          (escape "input: invalid length"))

        (define base-out-len (fx* full-quartets 3))
        (define out-len
          (cond
            [padded?
             (if (fx= (chunked-bytes-ref bs (fx- in-len 1)) (char->integer #\=))
                 (if (fx= (chunked-bytes-ref bs (fx- in-len 2)) (char->integer #\=))
                     (fx- base-out-len 2)
                     (fx- base-out-len 1))
                 base-out-len)]
            [else
             (case leftover-bytes
               [(0) base-out-len]
               [(2) (fx+ base-out-len 1)]
               [(3) (fx+ base-out-len 2)])]))

        (define-values [more-input-bytes? next-input-byte] (sequence-generate (in-chunked-bytes bs)))
        (define input-pos 0)
        (define (next-input-digit)
          (define digit (vector-ref BASE64-CHAR=>DIGIT (next-input-byte)))
          (cond
            [digit
             (set! input-pos (fx+ input-pos 1))
             digit]
            [else
             (escape/invalid-encoding input-pos)]))

        (define last-input-digit #f)

        ;; Stores the index of the next output byte, modulo 3.
        (define out-state 0)

        (define (next-output-byte)
          (case out-state
            [(0)
             (define d1 (next-input-digit))
             (define d2 (next-input-digit))
             (set! last-input-digit d2)
             (set! out-state 1)
             (fxior (fxlshift d1 2) (fxrshift d2 4))]
            [(1)
             (define d2 last-input-digit)
             (define d3 (next-input-digit))
             (set! last-input-digit d3)
             (set! out-state 2)
             (fxior (fxlshift (fxand d2 #b1111) 4) (fxrshift d3 2))]
            [else
             (define d3 last-input-digit)
             (define d4 (next-input-digit))
             (set! out-state 0)
             (fxior (fxlshift (fxand d3 #b11) 6) d4)]))

        (build-chunked-bytes out-len (λ (i) (next-output-byte)))))]))
